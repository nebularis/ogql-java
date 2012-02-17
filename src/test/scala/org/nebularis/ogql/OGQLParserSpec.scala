package org.nebularis.ogql

import ast._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{Inside, FlatSpec}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalacheck.Gen._
import org.scalacheck.Gen
import org.scalatest.prop.{PropertyChecks, GeneratorDrivenPropertyChecks, Checkers}

@RunWith(classOf[JUnitRunner])
class OGQLParserSpec extends FlatSpec
                     with GeneratorDrivenPropertyChecks
                     with PropertyChecks
                     with ShouldMatchers
                     with Inside {

    // basic AST serialisation

    it should "be reversible into a query (string) representation" in {
        val q = "a => b, c => c-d, c-e"
        val qs = parsed(q).queryString
        // info("qs: " + qs)
        qs should not equal (q)
        qs should equal ("(a => (b, (c => (c-d, c-e))))")
    }
    
    /*it should "produce a semantically equivalent representation" in {
        val qs =
            parsed("a-b => (((b-c => c-x), b-d) => (d-n, x-n))")
        info(qs.toString())
        info("now reparsing from: " + qs.queryString)
        info(parsed(qs.queryString).toString())
    }*/

    // basic predicate handling

    it should "consume all its inputs greedily" in {
        parsed("a")        should equal (EdgeTypePredicate("a"))
        parsed("abcdef")   should equal (EdgeTypePredicate("abcdef"))
    }

    it should "should map camel-cased words to the appropriate predicate node type" in {
        parsed("animal-genus")             should equal (EdgeTypePredicate("animal-genus"))
        parsed("person_ancestry")          should equal (EdgeTypePredicate("person_ancestry"))
        parsed("business2business")        should equal (EdgeTypePredicate("business2business"))
        parsed("Fruit")                    should equal (NodeTypePredicate("Fruit"))
        parsed("Golden_Delicious_Apple")   should equal (NodeTypePredicate("Golden_Delicious_Apple"))
        parsed("Savoy-Cabbage")            should equal (NodeTypePredicate("Savoy-Cabbage"))
    }

    it should "puke when passed invalid characters" in {
        forAll ("c", minSize(0)) { (c: String) =>
            whenever(c != null && !c.matches("^[\\w\\-_]*$")) {
                evaluating {
                    parsed(c)
                } should produce [ParseFailureException]
            }
        }
    }

    // TODO: filter expressions

    // basic groupings

    // TODO: extend these tests as much as possible

    it should "treat groupings as left associative" in {
        parsed("a => (b => c)")            should equal (parsed("a => b => c"))
        parsed("a => (b => (c => d))")     should equal (parsed("a => b => c => d"))
        parsed("a=>(b=>(c, d))")           should equal (parsed("a=>b=>c,d"))

        // demonstrate that the fixity of the join operators
        // can be controlled with grouping...

        // TODO: tighten this up, a lot...
        parsed("a-b => (((b-c => c-x), b-d) => (d-n, x-n))") should not equal (
            parsed("a-b => b-c => c-x, b-d => d-n, x-n")
        )
    }

    // join operations

    it should "distinguish between strict and non-strict 'intersect' join operators" in {
        forAll ((Gen.alphaStr, "l"),
            (Gen.alphaStr, "r"),
            maxDiscarded(100),
            minSize(2)) { (l: String, r: String) =>

            whenever(l.size > 0 && r.size > 0) {
                val j = chooseMaybeStrictJoinOperator
                // generate simple query := (a ~> b) OR (a => b)
                val intersection: Intersection = parsed(joinExpr(l, j, r)).asInstanceOf[Intersection]
                intersection should not be (null)
                intersection.strict match {
                    case true => j should be ("=>")
                    case false => j should be ("~>")
                }
            }
        }
    }

    it should "assign each axis of a join operation to the appropraite predicate type" in {
        inside(parsed("abc => cde")) { case Intersection(lhs, rhs) =>
            inside(lhs) { 
                case EdgeTypePredicate(leftNodeId) =>
                    leftNodeId should equal ("abc")
            }
            inside(rhs) {
                case EdgeTypePredicate(rightNodeId) =>
                    rightNodeId should equal ("cde")
            }
        }

        inside(parsed("a => ((b,c) => ((b => d), (c => d)))")) { case Intersection(_, outerRhs) =>
            inside(outerRhs) { case Intersection(lhs, rhs) =>
                inside(lhs) { case Union(b1, c1) =>
                    inside(b1) { case EdgeTypePredicate(b1Id) => b1Id should equal ("b") }
                    inside(c1) { case EdgeTypePredicate(c1Id) => c1Id should equal ("c") }
                }
                inside(rhs) { case Union(uLhs, uRhs) =>
                    inside(uLhs) { case Intersection(b2, d1) =>
                        inside(b2) { case EdgeTypePredicate(b2Id) => b2Id should equal ("b") }
                        inside(d1) { case EdgeTypePredicate(d1Id) => d1Id should equal ("d") }
                    }
                    inside(uRhs) { case Intersection(c2, d2) =>
                        inside(c2) { case EdgeTypePredicate(c2Id) => c2Id should equal ("c") }
                        inside(d2) { case EdgeTypePredicate(d2Id) => d2Id should equal ("d") }
                    }
                }
            }
        }
    }

    // TODO: pass-through join operations
    // TODO: existential join operations

    // sub-queries

    it should "apply sub-queries to any valid query expression" in {
        val queryConstructs =
            Table(
                ("l", "r", "s"),

                // TODO: URGENT: increase the coverage of these table-driven tests

                // ((intersect <- (intersect, edge-name)) => (intersect))
                ("(customer => order)",
                 "((order-shipment => shipment-carrier), order-completion)",
                  Left("(order-product => product-supplier)")),

                // ((*intersect <- (union)) => (edge-name))
                ("(*(personRoles => roleRelationship))",
                 "(staff-reumerationHistory)",
                  Left("(employee-contract, employee-roles)")),

                // ((*edge-name) => (edge-name <- union))
                ("(*manager-employee)",
                  "(staff-reumerationHistory)",
                  Right("(pay-details, pay-scale)")))
        
        forAll (queryConstructs) {
            (l: String,  r: String, s: Either[String, String]) =>
                s match {
                    case Left(sq) =>
                        inside(verboseParsing("((".concat(l).concat(" <- ")
                                         .concat(sq).concat(") => ")
                                         .concat(r).concat(")"))) {
                            case ast: JoinType =>
                                checkSubQuery(sq, ast.lhs)
                            case WithModifier(_, ast: JoinType) =>
                                checkSubQuery(sq, ast.lhs)
                            case _ =>
                                fail("Expected WithSubquery node nested on the LHS")
                        }
                    case Right(sq) =>
                        inside(verboseParsing("(".concat(l).concat(" => (")
                                         .concat(r).concat(" <- ")
                                         .concat(sq).concat("))"))) {
                            case ast: JoinType =>
                                checkSubQuery(sq, ast.rhs)
                            case WithModifier(_, ast: JoinType) =>
                                checkSubQuery(sq, ast.rhs)
                            case _ =>
                                fail("Expected WithSubquery node nested on the RHS")
                        }
                }
        }
    }

    it should "distinguish between strict and non-strict sub-queries" in {
        forAll ((Gen.alphaStr, "l"),
                (Gen.alphaStr, "r"),
                 maxDiscarded(100),
                 minSize(2)) { (l: String, r: String) =>

            whenever(l.size > 0 && r.size > 0) {
                val j = chooseMaybeStrictSubQueryOperator
                // generate simple query := (a ~> b) OR (a => b)
                inside(verboseParsing(joinExpr(l, j, r))) {
                    case withSubQuery: WithSubquery =>
                        withSubQuery.strict match {
                            case true => j should equal ("<-")
                            case false => j should equal ("<~")
                        }
                }
            }
        }
    }

    // traversal modifiers

    it should "support traversal modifiers in any valid join expression" in {
        forAll ((Gen.alphaStr, "l"),
                (Gen.alphaStr, "r"),
                maxDiscarded(100),
                minSize(2)) { (l: String, r: String) =>

            whenever(l.size > 0 && r.size > 0) {

                val t = chooseTraversalOperator
                val j = chooseJoinOperator

                // generate simple query := <t>(<l> <j> <r>), e.g., *(a => b)
                inside(parsed(traverseExpr(t, l, j, r))) {
                    case WithModifier(mod, ast) =>
                        mod.tokenString should equal (t)
                        inside(ast) { 
                            case Intersection(_1, _2) => j should be ("=>")
                            case Union(_a, _b) => j should be (",")
                        }
                }

                // generate query applying the traversal operator to the rhs,
                // e.g., `(a => !b)' or `(xs => *ys-xs)'
                inside(parsed(traverseRhs(t, l, j, r))) {
                    case Intersection(_, rhs) =>
                        inside(rhs) {
                            case WithModifier(mod, _) =>
                                mod.tokenString should equal (t)
                        }
                    case Union(_, rhs) =>
                        inside(rhs) {
                            case WithModifier(mod, _) =>
                                mod.tokenString should equal (t)
                        }
                }

                // ignore the rhs and generate a query against the lhs only
                inside(parsed(t.concat(l))) {
                    case WithModifier(mod, _) =>
                        mod.tokenString should equal (t)
                }
            }
        }
    }

    // TODO: result-set slicing

    // test utilities, custom generators and property/check configuration

    implicit override val generatorDrivenConfig =
        PropertyCheckConfig(minSize = 2, workers = 10)

    val padding = " "

    def checkSubQuery(sq: String, ast: Any) = ast match {
        case WithSubquery(leftNode, subQuery) =>
            info("Original: " + sq + "; Generated: " + subQuery.queryString)
            subQuery.queryString should equal (sq)
        case _ =>
            fail("Expected WithSubquery node nested on the LHS")
    }

    def traverseRhs(t: String, l: String, j: String, r: String) =
        traverseExpr("", l, j, t.concat(r))

    def joinExpr(l: String, j: String, r: String) =
        traverseExpr("", l, j, r)

    def traverseExpr(t: String, l: String, j: String, r: String) =
        List(t, "(", l, padding, j, padding,
                r, ")").foldLeft("")((x,y) => x.concat(y))

    def chooseJoinOperator =
        Gen.frequency((1, "=>"), (1, ",")).apply(Gen.Params()).get
    
    def chooseTraversalOperator =
        Gen.frequency((1, "!"),  (1, "*")).apply(Gen.Params()).get
    
    def chooseMaybeStrictJoinOperator =
        Gen.frequency((1, "=>"), (1, "~>")).apply(Gen.Params()).get

    def chooseMaybeStrictSubQueryOperator =
        Gen.frequency((1, "<-"), (1, "<~")).apply(Gen.Params()).get

    private def parsed(q: String) = {
        new OGQLParser().parseQuery(q)
    }

    private def verboseParsing(q:String) = {
        info("Parsing: " + q)
        val result =
            try {
                parsed(q)
            } catch {
                case e: ParseFailureException =>
                    info("Parse Error: " + e.msg + ": position=" + e.position)
                    throw e
            }
        info("Result: " + result.queryString)
        result
    }
}

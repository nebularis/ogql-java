package org.nebularis.ogql

import ast._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{Inside, FlatSpec}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalacheck.Gen._
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, Checkers}

@RunWith(classOf[JUnitRunner])
class OGQLParserSpec extends FlatSpec
                     with GeneratorDrivenPropertyChecks
                     with ShouldMatchers
                     with Inside {

    it should "consume all its inputs greedily" in {
        parsing("a")        should equal (EdgeTypePredicate("a"))
        parsing("abcdef")   should equal (EdgeTypePredicate("abcdef"))
    }
    
    it should "should map camel-cased words to the appropriate predicate node type" in {
        parsing("animal-genus")             should equal (EdgeTypePredicate("animal-genus"))
        parsing("person_ancestry")          should equal (EdgeTypePredicate("person_ancestry"))
        parsing("business2business")        should equal (EdgeTypePredicate("business2business"))
        parsing("Fruit")                    should equal (NodeTypePredicate("Fruit"))
        parsing("Golden_Delicious_Apple")   should equal (NodeTypePredicate("Golden_Delicious_Apple"))
        parsing("Savoy-Cabbage")            should equal (NodeTypePredicate("Savoy-Cabbage"))
    }

    it should "puke when passed invalid characters " in {
        forAll ("c", minSize(0)) { (c:String) =>
            whenever(c != null && !c.matches("^[\\w\\-_]*$")) {
                evaluating {
                    parsing(c)
                } should produce [ParseFailureException]
            }
        }
    }

    it should "treat groupings as left associative" in {
        parsing("a => (b => c)")            should equal (parsing("a => b => c"))
        parsing("a => (b => (c => d))")     should equal (parsing("a => b => c => d"))
        parsing("a=>(b=>(c, d))")           should equal (parsing("a=>b=>c,d"))

        // demonstrate that the fixity of the join operators
        // can be controlled with grouping...

        // TODO: tighten this up, a lot...
        parsing("a-b => (((b-c => c-x), b-d) => (d-n, x-n))") should not equal (
            parsing("a-b => b-c => c-x, b-d => d-n, x-nL")
        )
    }

    it should "assign each axis of a join operation to the appropraite predicate type" in {
        inside(parsing("abc => cde")) { case Intersection(lhs, rhs) =>
            inside(lhs) { 
                case EdgeTypePredicate(leftNodeId) =>
                    leftNodeId should equal ("abc")
            }
            inside(rhs) {
                case EdgeTypePredicate(rightNodeId) =>
                    rightNodeId should equal ("cde")
            }
        }

        inside(parsing("a => ((b,c) => ((b => d), (c => d)))")) { case Intersection(_, outerRhs) =>
            inside(outerRhs) { case Intersection(lhs, rhs) =>
                inside(lhs) { case Union(b1, c1) =>
                    inside(b1) { case EdgeTypePredicate(b1Id) => b1Id should equal ("b") }
                    inside(c1) { case EdgeTypePredicate(c1Id) => c1Id should equal ("c") } }
                inside(rhs) { case Union(uLhs, uRhs) =>
                    inside(uLhs) { case Intersection(b2, d1) =>
                        inside(b2) { case EdgeTypePredicate(b2Id) => b2Id should equal ("b") }
                        inside(d1) { case EdgeTypePredicate(d1Id) => d1Id should equal ("d") } }
                    inside(uRhs) { case Intersection(c2, d2) =>
                        inside(c2) { case EdgeTypePredicate(c2Id) => c2Id should equal ("c") }
                        inside(d2) { case EdgeTypePredicate(d2Id) => d2Id should equal ("d") } } } }
        }
    }

    it should "support negated traversal modifiers in any valid join expression" in {
        forAll ((Gen.alphaStr, "l"),
                (Gen.alphaStr, "r"),
                maxDiscarded(100),
                minSize(2)) { (l: String, r: String) =>

            whenever(l.size > 0 && r.size > 0) {
                val t = chooseTraversalOperator
                val j = chooseJoinOperator

                // generate simple query := <t>(<l> <j> <r>), e.g., *(a => b)
                inside(parsing(traverseExpr(t, l, j, r))) {
                    case WithModifier(mod, ast) =>
                        mod.tokenString should equal (t)
                        inside(ast) { 
                            case Intersection(_1, _2) => j should be ("=>")
                            case Union(_a, _b) => j should be (",")
                        }
                }

                // generate query applying the traversal operator to the rhs,
                // e.g., `(a => !b)' or `(xs => *ys-xs)'
                inside(parsing(traverseRhs(t, l, j, r))) {
                    case Intersection(_, rhs) =>
                        inside(rhs) {
                            case WithModifier(mod, _) =>
                                mod.tokenString should equal (t) }
                    case Union(_, rhs) =>
                        inside(rhs) {
                            case WithModifier(mod, _) =>
                                mod.tokenString should equal (t) }
                }

                // ignore the rhs and generate a query against the lhs only
                inside(parsing(t.concat(l))) {
                    case WithModifier(mod, _) =>
                        mod.tokenString should equal (t)
                }
            }
        }
    }

    // test utilities, custom generators and property/check configuration

    implicit override val generatorDrivenConfig =
        PropertyCheckConfig(minSize = 2, workers = 10)

    val padding = " "

    def traverseRhs(t: String, l: String, j: String, r: String) =
        traverseExpr("", l, j, t.concat(r))

    def traverseExpr(t: String, l: String, j: String, r: String) =
        List(t, "(", l, padding, j, padding,
                r, ")").foldLeft("")((x,y) => x.concat(y))

    def chooseJoinOperator =
        Gen.frequency((1, "=>"), (1, ",")).apply(Gen.Params()).get
    
    def chooseTraversalOperator =
        Gen.frequency((1, "!"),  (1, "*")).apply(Gen.Params()).get
    
    private def parsing(q: String) = {
        new OGQLParser().parseQuery(q)
    }

    private def verboseParsing(q:String) = {
        val result = parsing(q)
        println(result)
        result
    }
}

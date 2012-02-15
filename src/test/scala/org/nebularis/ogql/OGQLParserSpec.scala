package org.nebularis.ogql

import ast.{Union, NodeTypePredicate, EdgeTypePredicate, Intersection}
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{Inside, FlatSpec}
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

/**
 *
 */
@RunWith(classOf[JUnitRunner])
class OGQLParserSpec extends OGQLParser
                     with FlatSpec
                     with ShouldMatchers
                     with Inside {

    it should "consume all its inputs greedily" in {
        parsing("a")        should equal (EdgeTypePredicate("a"))
        parsing("abcdef")   should equal (EdgeTypePredicate("abcdef"))
    }
    
    it should "allow for all valid character sequences in a name based predicate" in {
        parsing("animal-genus")             should equal (EdgeTypePredicate("animal-genus"))
        parsing("person_ancestry")          should equal (EdgeTypePredicate("person_ancestry"))
        parsing("business2business")        should equal (EdgeTypePredicate("business2business"))
        parsing("Fruit")                    should equal (NodeTypePredicate("Fruit"))
        parsing("Golden_Delicious_Apple")   should equal (NodeTypePredicate("Golden_Delicious_Apple"))
        parsing("Savoy-Cabbage")            should equal (NodeTypePredicate("Savoy-Cabbage"))

        // TODO: move this to use a table or generator based property instead
        intercept[ParseFailureException] {
            verboseParsing("non^valid")
            verboseParsing("non!valid")
            verboseParsing("non#valid")
            verboseParsing("non£valid")
            verboseParsing("non&valid")
            verboseParsing("non*valid")
            verboseParsing("non(valid")
            verboseParsing("non)valid")
            verboseParsing("non/valid")
            verboseParsing("non`valid")
            verboseParsing("non|valid")
            verboseParsing("non\\valid")
            verboseParsing("non+valid")
            verboseParsing("non±valid")
            verboseParsing("non§valid")
            verboseParsing("non" + "\\\"" + "valid")
            verboseParsing("non'valid")
        }
    }

    it should "treat groupings as left associative" in {
        parsing("a => (b => c)")            should equal (parsing("a => b => c"))
        parsing("a => (b => (c => d))")     should equal (parsing("a => b => c => d"))
        // parsing("a-b => (((b-c => c-x), b-d) => (d-n, x-n))")
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

    private def parsing(q: String) = parseQuery(q)

    private def verboseParsing(q:String) = {
        val result = parsing(q)
        println(result)
        result
    }
}

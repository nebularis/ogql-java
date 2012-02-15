package org.nebularis.ogql

import ast.{NodeTypePredicate, EdgeTypePredicate, Intersection}
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
        parsing("Fruit")                    should equal (NodeTypePredicate("Fruit"))
        parsing("Golden_Delicious_Apple")   should equal (NodeTypePredicate("Golden_Delicious_Apple"))
        parsing("Savoy-Cabbage")            should equal (NodeTypePredicate("Savoy-Cabbage"))

        intercept[ParseFailureException] {
            verboseParsing("non^valid!characters$in&this*name;")
        }
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
    }

    private def parsing(q: String) = parseQuery(q)

    private def verboseParsing(q:String) = {
        val result = parsing(q)
        println(result)
        result
    }
}

package org.nebularis.ogql

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.{OneInstancePerTest, FeatureSpec, GivenWhenThen}
import org.scalatest.matchers.ShouldMatchers
import ast._

@RunWith(classOf[JUnitRunner])
class ParserFeatureSpec extends FeatureSpec
                        with ShouldMatchers
                        with GivenWhenThen
                        with OneInstancePerTest {

    val parser: QueryParser = new OGQLParser

    feature("Parsing Simple OGQL Queries") {
        info("As a developer")
        info("I want to parse simple OGQL queries")
        info("So that I can operate on the resulting AST")

        scenario("A query operating on a single edge type predicate") {
            val p = "catalogue-products"
            given("an edge type predicate query '".concat(p).concat("'"))
            when("the parser is invoked")
            then("the resulting AST should contain only the predicate")

            parser.parseQuery(p) should equal (EdgeTypePredicate(p))
        }

        scenario("A query operating on a single node type predicate") {
            val p = "Market"
            given("a node type predicate query '".concat(p).concat("'"))
            when("the parser is invoked")
            then("the resulting AST should contain only the predicate")

            parser.parseQuery(p) should equal (NodeTypePredicate(p))
        }
    }

    feature("Including nodes in a join, whilst excluding them from the results") {
        ignore("I want to find someone's grandparents") {
            val qs =
                "Person['Thor Odinsson'] => { person-ancestor } => person-ancestor"
        }
    }

    feature("Checking for the existence of something") {
        ignore("Finding Managers with no employees/staff-list") {
            val qs =
                "{ Role[Manager] } => " +
                  "Person => EMPTY(staff-list)"
                  "Person => EXISTS(staff-list)"
        }
    }

    feature("Parsing OGQL Join Queries") {
        info("As a developer")
        info("I want to parse OGQL join queries")
        info("So that I can apply them to object graphs at runtime")

        scenario("An OGQL 'intersect' query") {
            val (left, right) = ("customer-orders", "order-details")
            val q = left.concat(" => ").concat(right)

            given("an OGQL 'intersect' query: ".concat(q))
            when("the parser is invoked")
            then("the resulting AST should contain both parts")

            parser.parseQuery(q) should equal (
                Intersection(EdgeTypePredicate(left), EdgeTypePredicate(right)))
        }

        scenario("An OGQL 'union' query") {
            val (left, right) = ("customer-orders", "order-details")
            val q = left.concat(", ").concat(right)

            given("an OGQL 'union' query: ".concat(q))
            when("the parser is invoked")
            then("the resulting AST should contain both parts")

            parser.parseQuery(q) should equal (
                Union(EdgeTypePredicate(left), EdgeTypePredicate(right)))
        }
    }

    feature("Parsing OGQL Containing SubQueries") {
        info("As a developer")
        info("I want to parse OGQL queries that include subqueries")
        info("So that I can extract the sub-query and process it")

        scenario("A simple predicate with a sub-query") {
            val predicate = "Item"
            val (orders, suppliers) = ("item-orders", "item-suppliers")
            val subQuery = "(" + orders + ", " + suppliers + ")"
            val q = predicate.concat(" <- ").concat(subQuery)

            given("an OGQL query with sub-query: ".concat(q))
            when("the parser is invoked")
            then("the resulting AST should contain all parts of the AST")

            parser.parseQuery(q) should equal (
                WithSubquery(NodeTypePredicate(predicate),
                             Union(EdgeTypePredicate(orders),
                                   EdgeTypePredicate(suppliers))))
        }

        scenario("An OGQL 'intersect' query with a " +
                 "sub-query on the left nodeset") {

            // (customer-orders <- order-details) => order-statusEvents
            val (left, subQuery) = ("customer-orders", "order-details")
            val right = "order-statusEvents"
            val q = "(" + left.concat(" <- ").concat(subQuery) + ")" +
                    " => " + right

            given("an 'intersect' with a sub-query: ".concat(q))
            when("the parser is invoked")
            then("the resulting AST should contain both the original " +
                 "query, and the sub-query")

            parser.parseQuery(q) should equal (Intersection(
                WithSubquery(EdgeTypePredicate(left),
                             EdgeTypePredicate(subQuery)),
                EdgeTypePredicate(right)))
        }

        scenario("An OGQL 'intersect' query with a " +
                 "sub-query on the right nodeset") {

            // Server => (Host, DataCentre) <- RuleBase
            val svr = "Server"
            val (left, right) = ("Host", "DataCentre")
            val subQuery = "RuleBase"
            val q = svr + " => (" +
                        left.concat(",").concat(right) +
                            ") <- " + subQuery

            given("an 'intersect' with a right oriented sub-query: ".concat(q))
            when("the parser is invoked")
            then("the resulting AST should place the " +
                 "sub-query in the correct position")

            parser.parseQuery(q) should equal (Intersection(
                NodeTypePredicate(svr),
                WithSubquery(Union(NodeTypePredicate("Host"),
                                   NodeTypePredicate("DataCentre")),
                    NodeTypePredicate("RuleBase"))))
        }

        scenario("An OGQL 'intersect' query with a nested sub-query") {
            // Server => (Host <- (ServiceContract, EnvironmentConfig)) => Region
            val svr = "Server"
            val (left, subQuery) = ("Host", "(ServiceContract, EnvironmentConfig)")
            val right = "Region"
            val q = svr.concat(" => ")
                       .concat("(").concat(left)
                       .concat(" <- ").concat(subQuery)
                       .concat(") => ").concat(right)

            given("an 'intersect' with a nested sub-query: ".concat(q))
            when("the parser is invoked")
            then("the resulting AST should place the " +
                 "sub-query in the correct position")

            parser.parseQuery(q) should equal (
                Intersection(
                    NodeTypePredicate(svr),
                    Intersection(
                        WithSubquery(NodeTypePredicate("Host"),
                                     Union(NodeTypePredicate("ServiceContract"),
                                           NodeTypePredicate("EnvironmentConfig"))),
                        NodeTypePredicate("Region"))))
        }

        ignore("An OGQL 'union' query") {
            val (left, right) = ("customer-orders", "order-details")
            val q = left.concat(", ").concat(right)

            given("a single predicate query: ".concat(q))
            when("the parser is invoked")
            then("the resulting AST should contain both parts")

            parser.parseQuery(q) should equal (
                Union(EdgeTypePredicate(left), EdgeTypePredicate(right)))
        }
    }
}

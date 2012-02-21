package org.nebularis.ogql.ast

/**
 * Gives us an interface for turning ast nodes back into a query string
 */
trait QueryRepresentation {
    def queryString: String
}

/**
 * Provides a means of checking which modifiers are added
 * to which nodes once we've successfully built our AST
 */
trait Modifier extends QueryRepresentation {
    val tokenString: String = null
    override def queryString = tokenString
}

trait Predicate extends QueryRepresentation {
    def id: String
    override def queryString = id
}

trait JoinType extends QueryRepresentation {
    def lhs: QueryRepresentation
    def rhs: QueryRepresentation
    def operator: String
    override def queryString =
        "(".concat(lhs.queryString)
           .concat(operator)
           .concat(rhs.queryString)
           .concat(")")

    var strict: Boolean = true
}

// node classes

/**
 * The base class for all our ast nodes
 */
sealed abstract class AstNode extends QueryRepresentation

case class EdgeTypePredicate(id: String) extends AstNode with Predicate
case class NodeTypePredicate(id: String) extends AstNode with Predicate

case class AxisPredicate(id: String) extends AstNode with Predicate {
    override def queryString = "^".concat(super.queryString)
}

case class WildcardPredicate()
    extends AstNode with QueryRepresentation { def queryString = "?" }

case class Intersection(lhs: AstNode,
                        rhs: AstNode)
    extends AstNode with JoinType {

    def this(lhs: AstNode, rhs: AstNode, strict: Boolean) = {
        this(lhs, rhs)
        this.strict = strict
    }

    def operator = if (strict) { " => " } else { " ~> " }

    override def queryString = (lhs, rhs) match {
        case (Intersection(_, Exists(_)), _) =>
            queryStringExcludingOperator
        case (Intersection(_, Empty(_)), _) =>
            queryStringExcludingOperator
        case (_, Exists(_)) =>
            queryStringExcludingOperator
        case (_, Empty(_)) =>
            queryStringExcludingOperator
        case x =>
            super.queryString
    }

    private def queryStringExcludingOperator: String =
        "(".concat(lhs.queryString).concat(" ")
            .concat(rhs.queryString).concat(")")
}

case class Union(lhs: AstNode with QueryRepresentation,
                 rhs: AstNode with QueryRepresentation)
    extends AstNode with JoinType { def operator = ", " }

case class NegationModifier(override val tokenString: String)
    extends AstNode with Modifier

case class RecursionModifier(override val tokenString: String)
    extends AstNode with Modifier

case class WithModifier(mod: Modifier, node: AstNode) extends AstNode {
    override def queryString = mod.queryString.concat(node.queryString)
}

abstract sealed class Axis
object LeftAxis extends Axis
object RightAxis extends Axis

case class WithSubquery(node: AstNode, subNode: AstNode)
    extends AstNode with JoinType {

    def this(node: AstNode, subNode: AstNode,
             strict: Boolean, axis: Axis) = {
        this(node, subNode)
        this.strict = strict
        this.axis = axis
    }

    override def lhs = node
    override def rhs = subNode
    override def operator = strict match {
        case true => mkString("-")
        case false => mkString("~")
    }

    var axis: Axis = RightAxis

    private def mkString(x: String) = {
        " <" + List.fill(strokeLength)(x).foldRight(" ") {(x,y) => x+y}
    }

    private def strokeLength = axis match {
        case LeftAxis => 2
        case RightAxis => 1
    }
}

trait ExistentialQuantifier extends {
    def delimiter: String
    def query: AstNode
    def queryString = " <".concat(delimiter).concat(" ")
                          .concat(query.queryString)
                          .concat(" ").concat(delimiter)
                          .concat("> ")

}

case class Exists(query: AstNode) extends AstNode with ExistentialQuantifier {
    override def delimiter = "|"
}

case class Empty(query: AstNode) extends AstNode with ExistentialQuantifier {
    override def delimiter = ":"
}

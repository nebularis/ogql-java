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
    def operator = " => "

    var strict: Boolean = false
}

case class Union(lhs: AstNode with QueryRepresentation,
                 rhs: AstNode with QueryRepresentation)
    extends AstNode with JoinType { def operator = ", " }

case class NegationModifier(override val tokenString: String)
    extends AstNode with Modifier

case class RecursionModifier(override val tokenString: String)
    extends AstNode with Modifier

case class WithModifier(mod: Modifier, node: AstNode)
    extends AstNode with QueryRepresentation {
    override def queryString = mod.tokenString.concat(node.queryString)
}

case class WithSubquery(node: AstNode, subNode: AstNode)
    extends AstNode with JoinType {
    override def lhs = node
    override def rhs = subNode
    override def operator = " <- "
}

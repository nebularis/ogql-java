package org.nebularis.ogql.ast

/**
 * Provides a means of checking which modifiers are added
 * to which nodes once we've successfully built our AST
 */
trait Modifier { val tokenString: String = null }

trait QueryRepresentation {
    def queryString: String
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
        lhs.queryString.concat(operator).concat(lhs.queryString)
}

/**
 * The base class for all our ast nodes
 */
sealed abstract class AstNode

// node classes

case class EdgeTypePredicate(id: String) extends AstNode with Predicate
case class NodeTypePredicate(id: String) extends AstNode with Predicate

case class AxisPredicate(id: String) extends AstNode with Predicate {
    override def queryString = "^".concat(super.queryString)
}

case class WildcardPredicate()
    extends AstNode with QueryRepresentation { def queryString = "?" }

case class Intersection(lhs: AstNode with QueryRepresentation,
                        rhs: AstNode with QueryRepresentation)
    extends AstNode with JoinType { def operator = " => " }

case class Union(lhs: AstNode with QueryRepresentation,
                 rhs: AstNode with QueryRepresentation)
    extends AstNode with JoinType { def operator = ", " }

case class NegationModifier(override val tokenString: String)
    extends AstNode with Modifier

case class RecursionModifier(override val tokenString: String)
    extends AstNode with Modifier

case class WithModifier(mod: Modifier, node: AstNode with QueryRepresentation)
    extends AstNode with QueryRepresentation {
    override def queryString = mod.tokenString.concat(node.queryString)
}

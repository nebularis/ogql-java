package org.nebularis.ogql.ast

/**
 * The base class for all our ast nodes
 */
trait Modifier { val char: String = null }

sealed abstract class AstNode
case class EdgeTypePredicate(id: String) extends AstNode
case class NodeTypePredicate(id: String) extends AstNode
case class AxisPredicate(id: String) extends AstNode
case class WildcardPredicate() extends AstNode
case class Intersection(lhs: AstNode, rhs: AstNode) extends AstNode
case class Union(lhs: AstNode, rhs: AstNode) extends AstNode

case class NegationModifier(override val char: String) extends AstNode with Modifier
case class RecursionModifier(override val char: String) extends AstNode with Modifier
case class WithModifier(mod: Modifier, node: AstNode) extends AstNode

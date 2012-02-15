package org.nebularis.ogql.ast

/**
 * The base class for all our ast nodes
 */
sealed abstract class AstNode
case class EdgeTypePredicate(id: String) extends AstNode
case class NodeTypePredicate(id: String) extends AstNode
case class AxisPredicate(id: String) extends AstNode
case class WildcardPredicate() extends AstNode
case class Intersection(lhs: AstNode, rhs: AstNode) extends AstNode
case class Union(lhs: AstNode, rhs: AstNode) extends AstNode

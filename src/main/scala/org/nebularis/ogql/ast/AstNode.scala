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
    val tokenString: String = ""
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
        "(".concat(unwrappedQueryString).concat(")")

    def unwrappedQueryString =
        lhs.queryString
           .concat(operator)
           .concat(rhs.queryString)

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
}

case class Union(lhs: AstNode with QueryRepresentation,
                 rhs: AstNode with QueryRepresentation)
    extends AstNode with JoinType { def operator = ", " }

case class NegationModifier(override val tokenString: String)
    extends AstNode with Modifier

case class RecursionModifier(override val tokenString: String)
    extends AstNode with Modifier

object TraversalOnlyModifier extends AstNode with Modifier

case class WithModifier(mod: Modifier, node: AstNode) extends AstNode {
    override def queryString = mod match {
        case TraversalOnlyModifier =>
            node match {
                case jt: JoinType => "{".concat(jt.unwrappedQueryString).concat("}")
                case _ => "{".concat(node.queryString).concat("}")
            }
        case _ => mod.queryString.concat(node.queryString)
    }
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
    override def operator = rhs match {
        case Exists(_)  => mkString("=")
        case Empty(_)   => mkString("|")
        case _ => strict match {
            case true => mkString("-")
            case false => mkString("~")
        }
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

case class Exists(query: AstNode) extends AstNode {
    override def queryString = query.queryString
}

case class Empty(query: AstNode) extends AstNode {
    override def queryString = query.queryString
}

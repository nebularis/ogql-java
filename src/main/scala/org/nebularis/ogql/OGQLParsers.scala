/* -----------------------------------------------------------------------------
 * Copyright (c) 2002-2012 Tim Watson (watson.timothy@gmail.com)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 * -----------------------------------------------------------------------------
 */

package org.nebularis.ogql

import java.lang.String
import util.parsing.combinator.{PackratParsers, RegexParsers}
import org.nebularis.ogql.ast._
import util.parsing.input.Position

trait QueryParser {
    def parseQuery(q: String): AstNode
}

/**
 * This trait can be mixed in wherever you want OGQL parsing capabilities.
 */
trait OGQLParsers extends RegexParsers
                  with PackratParsers
                  with QueryParser {

    // primary API

    def parseQuery(q: String): AstNode = {
        val result: ParseResult[AstNode] =
            parseAll(query, q)
        result match {
            case Success(astRoot, _) => astRoot
            case e: NoSuccess =>
                throw new ParseFailureException(e.msg, e.next.pos, null)
        }
    }

    // grammar

    def query: Parser[AstNode] =
        ((intersection | existentialQuery | union | edgeSet) ~ (subQuery?)) ^^ {
            case q~sub => sub match {
                case None => q
                case Some((direction, strictness, ast)) =>
                    new WithSubquery(q, ast, strictness, direction)
            }  
        }

    def intersection = (edgeSet | existentialQuery) ~ intersectionOperator ~ query ^^ {
        case edgeSet~op~query =>
            op match {
                case "=>" => Intersection(edgeSet, query)
                case "~>" =>
                    val ast = Intersection(edgeSet, query)
                    ast.strict = false
                    ast
            }
    }

    // this needs to be split up somehow, so that a <| b |> c works

    def existentialQuery: Parser[AstNode] =
        intersectedPipe |
        passThroughPipe  |
        leftClosedPipe

    def intersectedPipe =
        edgeSet ~ existentialQuantifier ~ intersectionOperator ~ query ^^ {
            case lhs~exist~isOp~rhs => isOp match {
                    case "=>" =>
                        Intersection(applyExistentialJoinOperator(lhs, exist), rhs)
                    case "~>" =>
                        Intersection(new Intersection(lhs, exist, false), rhs)
                }
        }

    // a <| b |> c
    def passThroughPipe = edgeSet ~ existsQuantifier ~ query ^^ {
        case lhs~pipe~rhs =>
            Intersection(applyExistentialJoinOperator(lhs, pipe), rhs)
            /*pipe match {
                case Exists(_) => Intersection(Intersection(lhs, pipe), rhs)
                case Empty(_) => Intersection(WithSubquery(lhs, pipe), rhs)
            }*/
    }

    // abc <| def |>
    def leftClosedPipe = edgeSet ~ existentialQuantifier ^^ {
        case s~q => applyExistentialJoinOperator(s, q)
    }

    def union = edgeSet ~ "," ~ query ^^
        { case edgeSet~comma~query => Union(edgeSet, query) }

    def existentialQuantifier: Parser[AstNode with ExistentialQuantifier] =
        existsQuantifier | emptySetQuantifier
    
    def existsQuantifier = "<|" ~> edgeSet <~ "|>" ^^ {
        case q => Exists(q)
    }
    
    def emptySetQuantifier = "<:" ~> edgeSet <~ ":>" ^^ {
        case q => Empty(q)
    }

    def edgeSet: Parser[AstNode] =
        ((traversalModifier?) ~ (predicate | group)) ^^ {
        case mod~set => mod match {
            case None => set
            case Some(modifier) => WithModifier(modifier, set)
        }
    }

    def subQuery: Parser[(Axis, Boolean, AstNode)] =
        subQueryOperator ~ query ^^ {
            case operator~ast =>
                operator match {
                    case "<-"   => (RightAxis, true, ast)
                    case "<~"   => (RightAxis, false, ast)
                    case "<--"  => (LeftAxis, true, ast)
                    case "<~~"  => (LeftAxis, false, ast)
                }
        }

    def group = "(" ~> query <~ ")" ^^ { s => s }

    def predicate: Parser[AstNode] =
        edgeTypePredicate |
        nodeTypePredicate |
        wildcardPredicate

    def edgeTypePredicate =
        lowerCaseIdentifier ^^ { EdgeTypePredicate }

    def nodeTypePredicate =
        upperCaseIdentifier ^^ { NodeTypePredicate }

    def wildcardPredicate = "~" ^^ { case _ => WildcardPredicate() }

    // axisPredicate
    /*def axisPredicate =
        (regex("\\^^[A-Z]"r) ~ word) ^^ {
            case x~y => AxisPredicate(x + y)
        }*/

    def intersectionOperator = (strictJoin | nonStrictJoin)
    def strictJoin = literal("=>")
    def nonStrictJoin = literal("~>")

    def subQueryOperator = (strictSubQuery | nonStrictSubQuery)
    def strictSubQuery = literal ("<--") | literal("<-")
    def nonStrictSubQuery = literal("<~~") | literal("<~")

    def traversalModifier = negationModifier | recursionModifier

    def negationModifier = literal("!") ^^ { NegationModifier }

    def recursionModifier = literal("*") ^^ { RecursionModifier }

    def lowerCaseIdentifier =
        (regex("[a-z]"r) ~ word) ^^ { case x~y => x+y } | regex("[a-z]"r)

    def upperCaseIdentifier =
        (regex("[A-Z]"r) ~ word) ^^ { case x~y => x+y }

    def word = (regex("[\\w\\-_]+"r) *) ^^ {
        case l:List[String] => l.foldLeft("") { (acc, in) => acc + in }
    }

    // def string                  = "'" ~ "[^']*".r ~ "'"

    private def applyExistentialJoinOperator(
                    leftOperand: AstNode,
                    operator: AstNode with ExistentialQuantifier) =
        operator match {
        case Exists(_) => Intersection(leftOperand, operator)
        case Empty(_) => WithSubquery(leftOperand, operator)
    }
}

/**
 * Straight exposure of the OGQL parsing capability, made available as a class
 * in order to simplify use by Java application code.
 *
 * NB: Due to some internal state that is maintained in Scala's 'RegexParsers',
 * this class is <b>NOT THREAD SAFE</b> so <b>DO NOT</b> attempt to use it
 * in multiple threads.
 */
class OGQLParser extends OGQLParsers { }

class ParseFailureException(val msg: String, val position: Position, ex: Exception)
    extends RuntimeException(msg, ex)

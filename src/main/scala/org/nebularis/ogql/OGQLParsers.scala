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
        ((intersection | union | edgeSet) ~ (subQuery?)) ^^ {
            case q~sub => sub match {
                case None => q
                case Some((direction, strictness, ast)) =>
                    new WithSubquery(q, ast, strictness, direction)
            }  
        }

    // a => b
    def intersection = edgeSet ~ intersectionOperator ~ query ^^ {
        case edgeSet~op~query =>
            op match {
                case "=>" => Intersection(edgeSet, query)
                case "~>" =>
                    val ast = Intersection(edgeSet, query)
                    ast.strict = false
                    ast
            }
    }

    // a, b
    def union = edgeSet ~ unionOperator ~ query ^^
        { case edgeSet~comma~query => Union(edgeSet, query) }

    // predicate | { no-output-predicate }
    def edgeSet = ("{"?) ~ traversal ~ ("}"?) ^^ {
        case open~ast~close => (open, close) match {
            case (None, None) => ast
            case (Some(_), Some(_)) => WithModifier(TraversalOnlyModifier(), ast)
        }
    }

    // c-b => !a-b, *b-b <- (b-c => c-d)
    def traversal: Parser[AstNode] =
        ((traversalModifier?) ~ (predicate | group) ~ (subQuery?)) ^^ {
        case mod~set~sq => mod match {
            case None => sq match {
                case None => set
                case Some((direction, strictness, ast)) =>
                    new WithSubquery(set, ast, strictness, direction)
            }
            case Some(modifier) => sq match {
                case None => WithModifier(modifier, set)
                case Some((direction, strictness, ast)) =>
                    WithModifier(modifier,
                        new WithSubquery(set, ast, strictness, direction))
            }
        }
    }

    // strict (inner) join: a <- b, c
    // non-strict (outer) join: a <~ b, c
    // left axis binding: a <-- b, b <~~ c
    // exists: a <= b
    // empty (not exists): a <| b
    def subQuery: Parser[(Axis, Boolean, AstNode)] =
        subQueryOperator ~ query ^^ {
            case operator~ast =>
                operator match {
                    case "<-"   => (RightAxis, true, ast)
                    case "<~"   => (RightAxis, false, ast)
                    case "<--"  => (LeftAxis, true, ast)
                    case "<~~"  => (LeftAxis, false, ast)
                    case "<="   => (RightAxis, true, Exists(ast))
                    case "<|"   => (RightAxis, true, Empty(ast))
                }
        }

    def group = "(" ~> query <~ ")" ^^ { s => s }

    //
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
    
    def unionOperator = literal(",")

    def subQueryOperator = (strictSubQuery | nonStrictSubQuery | existentialQuantifier)
    def strictSubQuery = literal ("<--") | literal("<-")
    def nonStrictSubQuery = literal("<~~") | literal("<~")

    def existentialQuantifier = existsTest | emptyTest
    def existsTest = literal("<=")
    def emptyTest  = literal("<|")

    def traversalModifier = negationModifier | recursionModifier

    def negationModifier = literal("!") ^^ { NegationModifier }

    def recursionModifier = literal("*") ^^ { RecursionModifier }

    def lowerCaseIdentifier =
        (regex("[a-z]"r) ~ word) ^^ { case x~y => x+y } | regex("[a-z]"r)

    def upperCaseIdentifier =
        (regex("[A-Z]"r) ~ word) ^^ { case x~y => x+y }

    def word = (regex("[\\w\\-_]+"r) *) ^^ {
        case l: List[String] => l.foldLeft("") { (acc, in) => acc + in }
    }

    // def string                  = "'" ~ "[^']*".r ~ "'"

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

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

/**
 * This trait can be mixed in wherever you want OGQL parsing capabilities.
 */
trait OGQLParsers extends RegexParsers with PackratParsers {

    // primary API
    def parseQuery(q: String): AstNode = {
        val result: ParseResult[AstNode] = parseAll(query, q)
        result match {
            case Success(astRoot, _) => astRoot
            case e: NoSuccess =>
                throw new ParseFailureException(e.msg, e.next.pos, null)
        }
    }

    // grammar

    def query: Parser[AstNode with QueryRepresentation] =
        (intersection | union | edgeSet)

    def intersection = edgeSet ~ "=>" ~ query ^^
        { case edgeSet~arrow~query => Intersection(edgeSet, query) }

    def union = edgeSet ~ "," ~ query ^^
        { case edgeSet~comma~query => Union(edgeSet, query) }

    def edgeSet: Parser[AstNode with QueryRepresentation] =
        ((traversalModifier?) ~ (predicate | group)) ^^ {
        case mod~set => mod match {
            case None => set
            case Some(modifier) => WithModifier(modifier, set)
        }
    }

    def traversalModifier = negationModifier | recursionModifier
    
    def negationModifier = literal("!") ^^ { NegationModifier }

    def recursionModifier = literal("*") ^^ { RecursionModifier }

    def group = "(" ~> query <~ ")" ^^ { s => s }

    def predicate: Parser[AstNode with QueryRepresentation] =
        edgeTypePredicate |
        nodeTypePredicate |
        wildcardPredicate

    def edgeTypePredicate =
        lowerCaseIdentifier ^^ { EdgeTypePredicate }

    def nodeTypePredicate =
        upperCaseIdentifier ^^ { NodeTypePredicate }

    def wildcardPredicate = "?" ^^ { case _ => WildcardPredicate() }

    // axisPredicate
    /*def axisPredicate =
        (regex("\\^^[A-Z]"r) ~ word) ^^ {
            case x~y => AxisPredicate(x + y)
        }*/

    def lowerCaseIdentifier =
        (regex("[a-z]"r) ~ word) ^^ { case x~y => x+y } | regex("[a-z]"r)

    def upperCaseIdentifier =
        (regex("[A-Z]"r) ~ word) ^^ { case x~y => x+y }

    def word = (regex("[\\w\\-_]+"r) *) ^^ {
        case l:List[String] => l.foldLeft("") { (acc, in) => acc + in }
    }


    // def string                  = "'" ~ "[^']*".r ~ "'"
    // def space                   = """\s+"""r
}

/**
 * Straight exposure of the OGQL parsing capability, made available as a class
 * in order to simplify use by Java application code.
 *
 * NB: Due to some internal state that is maintained in scala's `RegexParsers`,
 * the class is <b>NOT THREAD SAFE</b> so <b>DO NOT</b> attempt to use this
 * class in multiple threads.

 */
class OGQLParser extends OGQLParsers { }

class ParseFailureException(msg: String, position: Position, ex: Exception)
    extends RuntimeException(ex)

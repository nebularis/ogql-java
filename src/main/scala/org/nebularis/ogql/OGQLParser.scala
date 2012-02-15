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
import scala.util.parsing.input.Position

class OGQLParser extends RegexParsers with PackratParsers {

    def parseQuery(q: String): AstNode = {
        val result: ParseResult[AstNode] = parse(query, q)
        result match {
            case Success(astRoot, _) => astRoot
            case e: NoSuccess => throw new ParseFailureException(e.msg, e.next.pos)
        }
    }

    // grammar

    def query: Parser[AstNode] = intersection | edgeSet

    def intersection = edgeSet ~ "=>" ~ query ^^
        { case edgeSet~arrow~query => Intersection(edgeSet, query) }

    def edgeSet = predicate | group

    def group = "(" ~> query <~ ")" ^^ { s => s }

    def predicate: Parser[AstNode] = edgeTypePredicate |
                                     nodeTypePredicate |
                                     wildcardPredicate |
                                     axisPredicate

    def edgeTypePredicate =
        lowerCaseIdentifier ^^ { case s => EdgeTypePredicate(s) }

    def nodeTypePredicate =
        (regex("[A-Z]"r) ~ word) ^^ { case x~y => NodeTypePredicate(x + y) }

    def wildcardPredicate = "?" ^^ { case _ => WildcardPredicate() }

    def axisPredicate =
        (regex("\\^^[A-Z]"r) ~ word) ^^ {
            case x~y => AxisPredicate(x + y)
        }

    def lowerCaseIdentifier =
        regex("[a-z]"r) | (regex("[a-z]"r) ~ word) ^^ { case x~y => x+y }

    def word = regex("[\\w\\-_]+"r)


    // def string                  = "'" ~ "[^']*".r ~ "'"
    // def space                   = """\s+"""r
}

class ParseFailureException(msg: String, position: Position) extends RuntimeException

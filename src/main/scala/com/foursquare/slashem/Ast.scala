// Copyright 2011-2012 Foursquare Labs Inc. All Rights Reserved.

package com.foursquare.slashem



/**
 * Abstract Syntax Tree used to represent queries.
 */
object Ast {

  val escapePattern = """\b(OR|AND|or|and)\b""".r

  /**
   * Ripped from lucene source code QueryParser.java
   * http://bit.ly/AzKzV3
   *
   * @param q Query string to sanitize
   */
  def escape(q: String): String = {
    val sb = new StringBuilder()
    q.trim.foreach{c: Char =>
      // These characters are part of the query syntax and must be escaped
      if (c == '\\' || c == '+' || c == '-' || c == '!' || c == '(' || c == ')' || c == ':'
        || c == '^' || c == '[' || c == ']' || c == '\"' || c == '{' || c == '}' || c == '~'
        || c == '*' || c == '?' || c == '|' || c == '&') {
        sb.append('\\')
      }
      // Added (not part of QueryParser.java)
      if (c != '\u2019') {
        sb.append(c)
      }
    }
    // Added (not part of QueryParser.java)
    escapePattern.replaceAllIn(sb.toString, m => {"\"" + m.group(0) + "\""})
  }

  /**
   * Return the input string in an escaped quotes
   *
   * @param q Query string to quote
   */
  def quote(q: String): String = "\"" + q + "\""

  /**
   * AbstractClause represents query clauses.
   */
  abstract class AbstractClause {
    /**
     * Returns the clause's Solr query format string representation
     */
    def extend(): String
    

    

    /**
     * Generate an OrClause from a list of clauses
     *
     * @param clauses a list of abstract clauses to OR together
     * @return OrClause inputs ORed together
     */
    def or(clauses: AbstractClause*): OrClause = {
      OrClause(this::clauses.toList)
    }

    /**
     * Generate an AndClause from a list of clauses
     *
     * @param clauses a list of abstract clauses to AND together
     * @return AbstractClause* ANDed together @see AndClause
     */
    def and(clauses: AbstractClause*): AndClause = {
      AndClause(this::clauses.toList)
    }
  }

  /**
   * Case class representing a list of clauses ORed together
   *
   * You can use a OrClause() to join two or more clauses with an OR
   */
  case class OrClause(clauses: List[AbstractClause]) extends AbstractClause {
    /** @inheritdoc */
    def extend(): String = {
      clauses.map(c => "(" + c.extend + ")").mkString(" OR ")
    }

    
  }

  /**
   * Case class representing a list of clauses ANDed together
   */
  case class AndClause(clauses: List[AbstractClause]) extends AbstractClause {
    /** @inheritdoc */
    def extend(): String = {
      clauses.map(c => "(" + c.extend() + ")").mkString(" AND ")
    }
    
  }

  /**
   * A 'Clause' is something of the form 'field:(query)'
   * @param plus Defaults to true, used to negate queries (by setting to false).
   */
  case class Clause[T](fieldName: String, query: Query[T], plus: Boolean = true) extends AbstractClause {
    /** @inheritdoc */
    def extend(): String = {
      val (q,boost) = query match {
        case Group(x) => (query,1)
        case Splat() => (query,1)
        case Boost(Group(x),b) => (Group(x),b)
        case Boost(x,b) => (Group(x),b)
        case _ => (Group(query),1)
      }
      // If a field does not have a name then do not attempt to specify it
      val qstr = fieldName match {
        case "" => q.extend()
        case x => x + ":" + q.extend()
      }
      val booleanQuery = plus match {
        case true => qstr
        //This is added as a work around for the lack of support of
        //pure negative queries (even though its partially supported
        //now it turns out they don't work so well when nested)
        case false => "(*:* -"+qstr+")"
      }
      //Boost as approraite
      boost match {
        case 1.0 => booleanQuery
        case x => booleanQuery+"^"+x
      }
    }
  }
  /**
   * Class representing a field that can have its score boosted.
   * @see ScoreBoost
   */
  case class Field(fieldName: String) extends ScoreBoost {
    def boost(): String = {
      fieldName
    }
  }

  /**
   * Class representing a boost (multiplier) for matches occuring in the
   * given field
   * @see ScoreBoost
   */
  case class WeightedField(fieldName: String, weight: Double = 1) extends ScoreBoost {
    def boost(): String = {
      weight match {
        case 1.0 => fieldName
        case x: Double => fieldName + "^" + x.toString
      }
    }

  }

  /**
   * A phrase weighted field. Results in a document scoring boost
   * @param pf Traditional phrase query
   * @param pf2 In edismax type queries two word shingle matches
   * @param pf3 In edismax type queries three word shingle matches
   */
  case class PhraseWeightedField(fieldName: String, weight: Double = 1,
                                 pf: Boolean, pf2: Boolean, pf3: Boolean) {
    def extend(): String = {
      weight match {
        case 1.0 => fieldName
        case x: Double => fieldName + "^" + x.toString
      }
    }
  }

  /**
   * Abstarct Query class that provides an API for common query operations.
   * @define extend
   * @define and
   * @define boost
   * @define elasticExtend
   * @define elasticFilter
   */
  abstract class Query[T]() {
    /**
     * @return String Query's Solr query format  string representation
     */
    def extend(): String
    /**
     * Combine two queries using an AND
     * @see And
     */
    def and(c: Query[T]): Query[T] = And(this, c)
    /**
     * Combine two queries using an Or
     * @see Or
     */
    def or(c: Query[T]): Query[T] = Or(this, c)
    /**
     * Boost a query by a weight
     * @see Boost
     */
    def boost(weight: Float): Query[T] = Boost(this, weight)

  }

  /**
   * A class that provides and API for boosting the score of a field.
   */
  abstract class ScoreBoost {
    /**
     * Solr field boost function
     */
    def boost(): String    
  }

  /**
   * Name doesn't have to be a field name for Solr
   * it could be "lat,lng". However for ES it must be
   * a field
   */
  case class GeoDist(name: String, lat: Double, lng: Double, distType: String = "") extends ScoreBoost {
    /** @inheritdoc */
    def boost(): String = {
      distType match {
        case "square" => "sqedist(%s,%s,%s)".format(lat,lng,name)
        case _ => "dist(2,%s,%s,%s".format(lat,lng,name)
      }
    }    
  }

  case class Recip(query: ScoreBoost, x: Int, y: Int, z: Int) extends ScoreBoost {
    /** @inheritdoc */
    def boost: String = "recip(%s,%d,%d,%d)".format(query.boost, x, y, z)
  }

  /**
   * An empty Query.
   */
  case class Empty[T]() extends Query[T] {
    /** @inheritdoc */
    def extend(): String = "\"\""
    /** @inheritdoc */    
  }

  /**
   * A phrase containing a query that is optionally escaped.
   *
   * Represents a contiguous series of words to be matched in that order.
   */
  case class Phrase[T](query: T, escapeQuery: Boolean = true) extends Query[T] {
    /** @inheritdoc */
    def extend(): String = {
      if (escapeQuery) {
        '"' + escape(query.toString) + '"'
      } else {
        '"' + query.toString + '"'
      }
    }
   
  }

  /**
   * A Phrase Prefix.
   * @see Phrase
   */
  case class PhrasePrefix[T](query: T, escapeQuery: Boolean = true) extends Query[T] {
    /** @inheritdoc */
    def extend(): String = {
      if (escapeQuery) {
        '"' + escape(query.toString) + '*' + '"'
      } else {
        '"' + query.toString + '*' + '"'
      }
    }
  }

  /**
   * A term query.  Used for queries that don't need to be analyzed
   *
   * By default, elasticFilter() will always be cached!
   */
  case class Term[T](query: Iterable[T], escapeQuery: Boolean = true, cached: Boolean = true) extends Query[T] {
    /** @inheritdoc */
    //def extend() = throw new UnimplementedException("Slashem does not support Term queries Solr")
    def extend(): String = {
      escapeQuery match {
        // hack to fix wrapping the queries in a List()
        case true => {
          val queries = query.map(q => {'"' + escape(q.toString) + '"'})
          queries.mkString(" OR ")
        }
        case false => '"' + query.mkString(" OR ") + '"'
      }
    }        
  }

  case class Range[T](q1: Query[T],q2: Query[T]) extends Query[T] {
    /** @inheritdoc */
    def extend(): String = {'[' + q1.extend() + " TO " + q2.extend() +']'}    
  }

  /**
   * A class representing a Bag of words style query
   */
  case class BagOfWords[T](query: T, escapeQuery: Boolean = true) extends Query[T] {
    /** @inheritdoc */
    def extend(): String = {
      if (escapeQuery) {
        escape(query.toString)
      } else {
        query.toString
      }
    }

  }

  /**
   * Class representing a semantic grouping of Queries
   */
  case class Group[T](items: Query[T]) extends Query[T] {
    /** @inheritdoc */
    def extend(): String = {"(%s)".format(items.extend)}
  }

  /**
   * Class representing queries ANDed together
   */
  case class And[T](queries: Query[T]*) extends Query[T] {
    /** @inheritdoc */
    def extend(): String = {
      "(" + queries.map(c => c.extend()).mkString(" AND ") + ")"
    }
  }
  /**
   * Case class representing a list of queries ORed together
   */
  case class Or[T](queries: Query[T]*) extends Query[T] {
    /** @inheritdoc */
    def extend(): String = {
      queries.map(c => c.extend()).mkString(" OR ")
    }
  }

  case class Splat[T]() extends Query[T] {
    /** @inheritdoc */
    def extend(): String = "*"   
  }

  /**
   * Class representing a Query boosted by a weight.
   */
  case class Boost[T](q: Query[T], weight: Float) extends Query[T] {
    /** @inheritdoc */
    def extend(): String = q.extend() + "^" + weight.toString

  }
}

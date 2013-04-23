### Original README
# [Original README](https://github.com/foursquare/slashem/blob/master/README.md)

# Slashem (SolrRogue)

Slashem (SolrRogue) is a type-safe internal Scala DSL for constructing and
executing find requests against SOLR. It is a rogue-like for solr. It is not
currently full expressive to SOLR's query functionality, but covers the main
use cases that we encountered.



## Examples

[QueryTest.scala](https://github.com/foursquare/slashem/blob/master/src/test/scala/com/foursquare/slashem/QueryTest.scala) contains sample queries and shows the corresponding query.
[SolrRogueTest.scala](https://github.com/foursquare/slashem/blob/master/src/test/scala/com/foursquare/slashem/SolrRogueTest.scala) countains some sample records.

A basic query against the SUserTest might look something like

    val q = SUserTest where (_.fullname eqs "jon")

This would do a phrase search for "jon" against the fullname field in SUserTest.
A more complex query might specify a different query parser like so:

    val q = SUserTest where (_.fullname eqs "holden") useQueryType("edismax")

Frequently with edismax queries you want to run your query against multiple fields
and or boost queries which match the entire phrase. The followingis an example of how
to do this:

    val q = SVenueTest where (_.default contains lols) useQueryType("edismax") phraseBoost(_.text, 1) phraseBoost(_.name,200) phraseBoost(_.aliases,25) queryField(_.text) queryField(_.ngram_name, 0.2) queryField(_.tags, 0.01) tieBreaker(0.2)


### Quick simple try
 ```scala
package com.foursquare.slashem

import com.twitter.util.Duration
import java.util.concurrent.TimeUnit

object Test extends App {
	object STipTest extends STipTest with SolrMeta[STipTest] {
  //The name is used to determine which props to use.
		def solrName = "tips"
				//The servers is a list used in round-robin for running solr read queries against.
				def servers = List("localhost:8983")
	}
	class STipTest extends SolrSchema[STipTest] {
	  def meta = STipTest
	
	  object id extends SlashemStringField(this)
	  object name extends SlashemStringField(this)
	  object price extends SlashemStringField(this)
	 // object geo_s2_cell_ids extends SlashemGeoField(this)
	
	}
	val q =STipTest where (_.id eqs "GB18030TEST")
	println(q.meta)
	val s=q.fetchFuture().get(Duration(10, TimeUnit.SECONDS))
	s.map(r => r.response.results.map(rr => println(rr.name)))
}
```
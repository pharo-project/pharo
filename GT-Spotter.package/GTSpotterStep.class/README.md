It models one step in the spotter search.

A step knows:
- the origin object 
- the active processors that handle the query, and the candidates
- the selected candidate, and
- the list of all filtered candidates

It also knows the stream through which the list of candidates is affected.

The key method is process:
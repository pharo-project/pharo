The base rule for smalltalk code pattern matching (relies on rewrite expressions). The rule operates on AST nodes.

Use the following methods in the initialization to setup your subclass:

- matches:
- addMatchingExpression:
	add a string of rewrite expression to be matched by rule

- matchesAny:
	same as previous but takes a collection of strings to match
	
- addMatchingMethod:
	add a string of rewrite expression which should be parsed as a method
	
you may use #afterCheck:mappings: to do a post-matching validation of a matched node and mapping of wildcards.
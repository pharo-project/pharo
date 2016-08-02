as yet unclassified
rewriteRules 

	^ HelpTopic
		title: 'Rewrite Rules'
		contents: 
'To convert parse tree rules (subclasses of RBTransformationRule) please change their superclass to ReNodeRewriteRule.

Then change the initialization method. Instead of sending transformation-specifying methods to #rewriteRule, send them to self. The rest of API is similar:
#replace:with: -> #replace:with: (noChange)
#replaceMethod:with: -> #addMatchingMethod:rewriteTo:
? -> #replace:by: (second argument is a block which accepts matched node and returns a node that should be used for replacement).

So the old initialization:
	self rewriteRule
		replace: 	''`var := `var'' with: ''''
		
will become:
	self replace: 	''`var := `var'' with: ''''


The new rules also give you a move powerful way of post-checking matched nodes. You can override #afterCheck:mappings: method and return true if node really violates the rule or false otherwise. The first argument passed to the method is the matched node object, while the second argument is a dictionary of bindings for the wildcards in the rule. For example if the pattern ''`var := `var'' will match expression ''a := a'' the matches dictionary will contain one entry where key is RBPatternVariableNode(`var) and value is RBVariableNode(a).
P.S. at the moment of writing the matches dictionary was not used in a real setting, so don''t hesitate to provide a feedback. Maybe instead of node objects the dictionary should contain strings.'
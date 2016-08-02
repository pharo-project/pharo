as yet unclassified
parsetreeRules 

	^ HelpTopic
		title: 'Parse Tree Rules'
		contents: 
'To convert parse tree rules (subclasses of RBParseTreeLintRule) please change their superclass to ReNodeMatchRule.

Then change the initialization method. Instead of sending match-specifying methods to #matcher, send them to self. The rest of API is similar:
#matches:do: -> #matches:
#matchesMethod:do: -> #addMatchingMethod:
#matchesAnyOf:do: -> #matchesAny:

So the old initialization:
	self matcher 
		matches: ''`var := `var''
		do: [ :node :answer | node ]
		
will become:
	self matches: ''`var := `var''

You have noticed that new API is missing the "do:" part. First of all almost no rules use this functionality and you can check node in the matching expression with `{:node | "check node" } syntax.

But the new rules also give you a move powerful way of post-checking matched nodes. You can override #afterCheck:mappings: method and return true if node really violates the rule or false otherwise. The first argument passed to the method is the matched node object, while the second argument is a dictionary of bindings for the wildcards in the rule. For example if the pattern ''`var := `var'' will match expression ''a := a'' the matches dictionary will contain one entry where key is RBPatternVariableNode(`var) and value is RBVariableNode(a).
P.S. at the moment of writing the matches dictionary was not used in a real setting, so don''t hesitate to provide a feedback. Maybe instead of node objects the dictionary should contain strings.'
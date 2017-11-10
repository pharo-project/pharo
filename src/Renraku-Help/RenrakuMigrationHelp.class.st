"
Lately Pharo tools moved to Renraku framework which requires a slightly different implementation from rules.
	
	While you can achieve much more features by reading the whole documentation and using the complete set of Renraku possibilities, this book contains a few simple steps to help you converting existing rules to work with Renraku model.
"
Class {
	#name : #RenrakuMigrationHelp,
	#superclass : #RenrakuHelp,
	#category : #Renraku-Help
}

{ #category : #'as yet unclassified' }
RenrakuMigrationHelp class >> bookDescription [
	^ 'Lately Pharo tools moved to Renraku framework which requires a slightly different implementation from rules.
	
	While you can achieve much more features by reading the whole documentation and using the complete set of Renraku possibilities, this book contains a few simple steps to help you converting existing rules to work with Renraku model.'
]

{ #category : #'as yet unclassified' }
RenrakuMigrationHelp class >> bookName [
	    ^ 'Migrating Rules To Renraku'
]

{ #category : #'as yet unclassified' }
RenrakuMigrationHelp class >> genericRules [ 

	^ HelpTopic
		title: 'Generic Rules'
		contents: 
'To convert a generic rule (one that simply checks method, class or package) to work with Renraku you have to follow 3 simple steps:

Inheritance:
change superclass to ReAbstractRule.

Interest:
Specify which entities your rules is interested in by overriding a method on the class side to return true. If the rule was checking methods override #checksMethod, for classes override #checksClass, and for packages — #checksPackage.

Checking:
Implement #basicCheck: in a way that it will return true if the argument violated the rule and false otherwise. The quality tools will pass you only the arguments of the type you''ve expressed interest in.'
]

{ #category : #'as yet unclassified' }
RenrakuMigrationHelp class >> pages [
	    ^ #(genericRules parsetreeRules rewriteRules)
]

{ #category : #'as yet unclassified' }
RenrakuMigrationHelp class >> parsetreeRules [ 

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
]

{ #category : #'as yet unclassified' }
RenrakuMigrationHelp class >> rewriteRules [ 

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
]

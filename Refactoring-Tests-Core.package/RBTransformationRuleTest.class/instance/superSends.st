rules
superSends
	| rule |
	rule := RBParseTreeRewriter new.
	rule addSearch: 'super `@message: ``@args'
				-> (
					[:aNode | 
					(class withAllSubclasses
						detect: [:each | each includesSelector: aNode selector]
						ifNone: [nil]) isNil]
							-> 'self `@message: ``@args').
	self rewriteUsing: rule
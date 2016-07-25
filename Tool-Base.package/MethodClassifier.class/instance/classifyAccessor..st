classification-rules
classifyAccessor: aMethod
	" If the method is a setter or getter for a  "
	| names selector |
	
	names := aMethod methodClass allInstVarNames.
	selector := aMethod selector.
	
	(selector endsWith: ':')
		ifTrue: [ "selector might be a setter"
			selector := selector allButLast ].
	
	(names includes: selector)
		ifFalse: [ ^ false ].
		
	aMethod protocol: 'accessing'.
	^ true.
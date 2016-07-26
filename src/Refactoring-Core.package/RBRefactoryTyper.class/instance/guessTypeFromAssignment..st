assignments
guessTypeFromAssignment: aNode 
	| type set newType |
	type := nil.
	aNode value isAssignment 
		ifTrue: 
			[^self guessTypeFromAssignment: (RBAssignmentNode variable: aNode variable
						value: aNode value value)].
	aNode value isBlock ifTrue: [type := model classFor: [] class].
	aNode value isLiteralNode 
		ifTrue: 
			[aNode value value isNil ifTrue: [^self].
			type := model classFor: (self typeFor: aNode value value)].
	aNode value isMessage 
		ifTrue: 
			[aNode value receiver isVariable 
				ifTrue: [type := model classNamed: aNode value receiver name asSymbol].
			aNode value selector = #asValue 
				ifTrue: [type := model classNamed: #ValueHolder].
			(#(#and: #or: #= #== #~= #~~ #<= #< #~~ #> #>=) 
				includes: aNode value selector) ifTrue: [type := model classFor: Boolean]].
	type isNil ifTrue: [^self].
	set := variableTypes at: aNode variable name.
	newType := set detect: [:each | type includesClass: each] ifNone: [nil].
	newType isNil ifTrue: [^self].
	((self rootClasses includes: newType) or: [ newType = (model classFor: Object) ])
		ifTrue: [newType := type].
	(bestGuesses at: aNode variable name ifAbsentPut: [Set new]) add: newType
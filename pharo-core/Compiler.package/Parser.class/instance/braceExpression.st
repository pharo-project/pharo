expression types
braceExpression
	" { elements } => BraceNode."

	| elements locations loc more |
	elements := OrderedCollection new.
	locations := OrderedCollection new.
	self advance.
	more := hereType ~~ #rightBrace.
	[more]
		whileTrue: 
			[loc := hereMark + requestorOffset.
			self expression
				ifTrue: 
					[elements addLast: parseNode.
					locations addLast: loc]
				ifFalse:
					[^self expected: 'Variable or expression'].
			(self match: #period)
				ifTrue: [more := hereType ~~ #rightBrace]
				ifFalse: [more := false]].
	parseNode := BraceNode new elements: elements sourceLocations: locations.
	(self match: #rightBrace)
		ifFalse: [^self expected: 'Period or right brace'].
	^true
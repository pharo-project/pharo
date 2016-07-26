public
next
	| object |
	self consumeWhitespace.
	object := self parseValue.
	unresolvedReferences > 0
		ifTrue: [ self processSubObjectsOf: object ].
	unresolvedReferences = 0
		ifFalse: [ self error: 'Inconsistent reference resolution' ].
	^ object
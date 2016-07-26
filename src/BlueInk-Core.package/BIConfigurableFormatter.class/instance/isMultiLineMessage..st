private
isMultiLineMessage: aMessageNode
	(self multiLineMessages includes: aMessageNode selector)
		ifTrue: [ ^ true ].
	(self oneLineMessages includes: aMessageNode selector)
		ifTrue: [ ^ false ].
	self numberOfArgumentsForMultiLine <= aMessageNode arguments size
		ifTrue: [ ^ true ].
	(self hasAMultiLineMessageArgument: aMessageNode arguments)
		ifTrue: [ ^ true ].
	aMessageNode isUnary
		ifTrue: [ ^ self isLineTooLong: aMessageNode selector ].
	^ self isLineTooLong: (self formatMessageNodeInAMessageStreamFor: aMessageNode) contents
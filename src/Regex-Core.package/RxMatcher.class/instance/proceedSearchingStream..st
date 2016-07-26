private
proceedSearchingStream: aStream

	| position |
	position := aStream position.
	[aStream atEnd] whileFalse:
		[self tryMatch ifTrue: [^true].
		aStream position: position; next.
		position := aStream position].
	"Try match at the very stream end too!"
	self tryMatch ifTrue: [^true]. 
	^false
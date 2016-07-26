accessing
searchStream: aStream
	"Search the stream for occurrence of something matching myself.
	After the search has occurred, stop positioned after the end of the
	matched substring. Answer a Boolean indicating success."

	| position |
	stream := aStream.
	position := aStream position.
	markerPositions := nil.
	[aStream atEnd] whileFalse:
		[self tryMatch ifTrue: [^true].
		aStream position: position; next.
		position := aStream position].
	"Try match at the very stream end too!"
	self tryMatch ifTrue: [^true]. 
	^false
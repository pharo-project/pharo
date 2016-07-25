match enumeration
copyStream: aStream to: writeStream replacingMatchesWith: aString
	"Copy the contents of <aStream> on the <writeStream>, except for the matches. Replace each match with <aString>."

	| searchStart matchStart matchEnd |
	stream := aStream.
	markerPositions := nil.
	[searchStart := aStream position.
	self proceedSearchingStream: aStream] whileTrue:
		[matchStart := (self subBeginning: 1) first.
		matchEnd := (self subEnd: 1) first.
		aStream position: searchStart.
		searchStart to: matchStart - 1 do:
			[:ignoredPos | writeStream nextPut: aStream next].
		writeStream nextPutAll: aString.
		aStream position: matchEnd.
		"Be extra careful about successful matches which consume no input.
		After those, make sure to advance or finish if already at end."
		matchEnd = searchStart ifTrue: 
			[aStream atEnd
				ifTrue:	[^self "rest after end of whileTrue: block is a no-op if atEnd"]
				ifFalse:	[writeStream nextPut: aStream next]]].
	aStream position: searchStart.
	[aStream atEnd] whileFalse: [writeStream nextPut: aStream next]
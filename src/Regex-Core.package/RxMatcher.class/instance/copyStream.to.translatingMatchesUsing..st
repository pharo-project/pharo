match enumeration
copyStream: aStream to: writeStream translatingMatchesUsing: aBlock
	"Copy the contents of <aStream> on the <writeStream>, except for the matches. For each match, evaluate <aBlock> passing the matched substring as the argument.  Expect the block to answer a String, and write the answer to <writeStream> in place of the match."

	| searchStart matchStart matchEnd match |
	stream := aStream.	
	markerPositions := nil.
	[searchStart := aStream position.
	self proceedSearchingStream: aStream] whileTrue:
		[matchStart := (self subBeginning: 1) first.
		matchEnd := (self subEnd: 1) first.
		aStream position: searchStart.
		searchStart to: matchStart - 1 do:
			[:ignoredPos | writeStream nextPut: aStream next].
		match := (String new: matchEnd - matchStart + 1) writeStream.
		matchStart to: matchEnd - 1 do:
			[:ignoredPos | match nextPut: aStream next].
		writeStream nextPutAll: (aBlock value: match contents).
		"Be extra careful about successful matches which consume no input.
		After those, make sure to advance or finish if already at end."
		matchEnd = searchStart ifTrue: 
			[aStream atEnd
				ifTrue:	[^self "rest after end of whileTrue: block is a no-op if atEnd"]
				ifFalse:	[writeStream nextPut: aStream next]]].
	aStream position: searchStart.
	[aStream atEnd] whileFalse: [writeStream nextPut: aStream next]
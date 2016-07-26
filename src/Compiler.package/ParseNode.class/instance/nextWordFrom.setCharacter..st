private
nextWordFrom: aStream setCharacter: aBlock
	| outStream char |
	outStream := (String new: 16) writeStream.
	[(aStream peekFor: Character space) 
		or: [aStream peekFor: Character tab]] whileTrue.
	[aStream atEnd
		or:
			[char := aStream next.
			char = Character cr or: [char = Character space]]]
		whileFalse: [outStream nextPut: char].
	aBlock value: char.
	^ outStream contents
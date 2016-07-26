accessing
parseLiterals: aString 
	| stream |
	stream := (Array new: 5) writeStream.
	[self atEnd or: [currentToken isSpecial and: [currentToken value = $)]]] 
		whileFalse: [stream nextPut: self parseLiteralArrayObject].
	
	self atEnd ifFalse: [ ^ self parserError: 'Unknown input at end'].
	
	^stream contents collect: [ :each | each value ]
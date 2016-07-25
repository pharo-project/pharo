parsing-internal
parseString
	| result |
	self expect: '"'.
	result := WriteStream on: String new.
	[ stream atEnd or: [ stream peek = $" ] ] 
		whileFalse: [ result nextPut: self parseCharacter ].
	^ self expect: '"'; createString: result contents
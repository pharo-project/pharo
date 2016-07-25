expression types
scanLitByte
	| stream |
	stream := (ByteArray new: 16) writeStream.
	[ tokenType = #rightBracket or: [ tokenType = #doIt ] ] whileFalse: [
		tokenType = #word
			ifTrue: [ self scanLitWord ].
		(token isInteger and: [ token between: 0 and: 255 ])
			ifFalse: [ ^ self offEnd: '8-bit integer or right bracket expected' ].
		stream nextPut: token.
		self scanToken ].
	token := stream contents
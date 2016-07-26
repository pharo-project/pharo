converting
asUUID: aString 
	| stream token byte |
	stream := (aString 
		copyReplaceAll: '-'
		with: '') asUppercase readStream.
	1 
		to: stream size / 2
		do: 
			[ :i | 
			token := stream next: 2.
			byte := Integer 
				readFrom: token readStream
				base: 16.
			self 
				at: i
				put: byte ].
	^ self
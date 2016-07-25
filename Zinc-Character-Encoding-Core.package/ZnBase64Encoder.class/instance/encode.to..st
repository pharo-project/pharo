converting
encode: byteStream to: stringStream
	| byte1 byte2 byte3 count |
	lineLength
		ifNil: [ 
			[ byteStream atEnd ] whileFalse: [ 
				byte1 := byteStream next.
				byte2 := byteStream next.
				byte3 := byteStream next.
				self encode: byte1 and: byte2 and: byte3 to: stringStream ] ]
		ifNotNil: [ 
			count := 0.
			[ byteStream atEnd ] whileFalse: [ 
				byte1 := byteStream next.
				byte2 := byteStream next.
				byte3 := byteStream next.
				self encode: byte1 and: byte2 and: byte3 to: stringStream.
				(count := count + 4) = lineLength
					ifTrue: [ 
						stringStream nextPutAll: lineEnd.
						count := 0 ] ] ]
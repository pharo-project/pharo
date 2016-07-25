converting
encode: readStream to: writeStream
	| bytes buffer byte |
	buffer := (bytes := ByteArray new: 4) writeStream.
	self safeSet; characterEncoder.
	[ readStream atEnd ]
		whileFalse: [ 
			buffer reset.
			characterEncoder nextPut: readStream next toStream: buffer.
			1 to: buffer position do: [ :index | 
				(safeSet includes: (byte := bytes at: index))
					ifTrue: [ writeStream nextPut: byte asCharacter ]
					ifFalse: [ 
						writeStream nextPut: $%.
						self writeHex: byte to: writeStream ] ] ]
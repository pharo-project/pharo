private
next: count putAllASCII: string startingAt: offset toStream: stream
	"Write count bytes from string starting at offset to stream,
	assuming all characters are in the ASCII set and need no translation"
	
	"For now, we don't use the option of mixing ByteString and ByteArray as similar"
	
	| bytes |
	bytes := ByteArray new: count.
	bytes replaceFrom: 1 to: count with: string startingAt: offset.
	stream nextPutAll: bytes
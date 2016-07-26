private
convertTextUtf8: aString
	| encoder buffer |
	buffer := ByteArray new: (aString size + 2) * 4.
	encoder := ZnUTF8Encoder new.
	encoder
		next: aString size
		putAll: aString
		startingAt: 1
		toStream: buffer writeStream.
	^ buffer
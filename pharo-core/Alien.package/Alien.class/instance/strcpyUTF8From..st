accessing
strcpyUTF8From: startIndex "<Integer> ^<String>"
	| length bytes |
	length := self strlenStartingAt: startIndex.
	self copyInto: (bytes := ByteArray new: length)
		from: 1 to: length
		in: self startingAt: startIndex.
	^bytes utf8ToIso
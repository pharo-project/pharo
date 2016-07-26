accessing
strcpyFrom: startIndex "<Integer> ^<ByteString>"
	| length string |
	length := self strlenStartingAt: startIndex.
	self copyInto: (string := ByteString new: length)
		from: 1 to: length
		in: self startingAt: startIndex.
	^string
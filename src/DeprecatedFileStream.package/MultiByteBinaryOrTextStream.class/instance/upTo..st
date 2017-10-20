accessing
upTo: delim 

	| out ch |
	out := (String new: 1000) writeStream.
	self atEnd ifTrue: [^ ''].
	[(ch := self next) isNil] whileFalse: [
		(ch = delim) ifTrue: [
			^ out contents  "terminator is not doubled; we're done!"
		].
		out nextPut: ch.
	].
	^ out contents.

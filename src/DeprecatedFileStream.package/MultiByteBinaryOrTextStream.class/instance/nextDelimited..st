accessing
nextDelimited: terminator

	| out ch pos |
	out := (String new: 1000) writeStream.
	self atEnd ifTrue: [^ ''].
	pos := self position.
	self next = terminator ifFalse: [
		"absorb initial terminator"
		self position: pos.
	].
	[(ch := self next) == nil] whileFalse: [
		(ch = terminator) ifTrue: [
			self peek = terminator ifTrue: [
				self next.  "skip doubled terminator"
			] ifFalse: [
				^ out contents  "terminator is not doubled; we're done!"
			].
		].
		out nextPut: ch.
	].
	^ out contents.

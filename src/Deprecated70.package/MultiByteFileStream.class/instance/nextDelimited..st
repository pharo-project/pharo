accessing
nextDelimited: terminator

	| out ch save |
	out := (String new: 1000) writeStream.
	self atEnd ifTrue: [^ ''].
	save := converter saveStateOf: self.

	self next = terminator ifFalse: [
		"absorb initial terminator"
		converter restoreStateOf: self with: save.
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

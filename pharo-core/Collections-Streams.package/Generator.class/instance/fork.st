private
fork
	| result |
	home := thisContext.
	block reentrant value: self.
	thisContext swapSender: continue.
	result := next.
	continue := next := home := nil.
	^ result
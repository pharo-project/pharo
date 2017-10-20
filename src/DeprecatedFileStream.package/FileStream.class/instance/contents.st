accessing
contents
	"Return the contents of the receiver. Do not close or otherwise touch the receiver. Return data in whatever mode the receiver is in (e.g., binary or text)."
	| s savePos |
	savePos := self position.
	self position: 0.
	s := self next: self size.
	self position: savePos.
	^s
truncation and round off
floor
	"Answer the integer nearest the receiver toward negative infinity."

	| truncation |
	truncation := self truncated.
	self >= 0 ifTrue: [^truncation].
	self = truncation
		ifTrue: [^truncation]
		ifFalse: [^truncation - 1]
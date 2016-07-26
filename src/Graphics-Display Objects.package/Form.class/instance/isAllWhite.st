testing
isAllWhite
	"Answer whether all bits in the receiver are white"
	| word |
	self unhibernate.
	word := Color white pixelWordForDepth: self depth.
	1 to: bits size do: [:i | (bits at: i) = word ifFalse: [^ false]].
	^ true
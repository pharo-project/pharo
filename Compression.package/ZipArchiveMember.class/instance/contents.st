reading
contents
	"Answer my contents as a string."
	| s |
	s := RWBinaryOrTextStream on: (String new: self uncompressedSize).
	self extractTo: s.
	s text.
	^s contents
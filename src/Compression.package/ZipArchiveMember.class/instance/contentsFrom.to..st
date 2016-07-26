reading
contentsFrom: start to: finish
	"Answer my contents as a string."
	| s |
	s := RWBinaryOrTextStream on: (String new: finish - start + 1).
	self extractTo: s from: start to: finish.
	s text.
	^s contents
testing
isAllAlphaNumerics
	"whether the receiver is composed entirely of alphanumerics"
	self do: [:c | c isAlphaNumeric ifFalse: [^ false]].
	^ true
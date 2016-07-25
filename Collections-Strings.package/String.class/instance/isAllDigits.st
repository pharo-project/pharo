testing
isAllDigits
	"whether the receiver is composed entirely of digits and has at least one digit"

	self do: [:c | c isDigit ifFalse: [^ false]].
	self ifEmpty: [^false].
       ^ true
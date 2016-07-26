testing
isAllSeparators
	"whether the receiver is composed entirely of separators"
	self do: [ :c | c isSeparator ifFalse: [ ^false ] ].
	^true
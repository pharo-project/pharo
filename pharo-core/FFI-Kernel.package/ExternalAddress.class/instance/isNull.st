testing
isNull
	"Answer true if I am a null pointer"
	1 to: self size do:[:i| (self at: i) = 0 ifFalse:[^false]].
	^true
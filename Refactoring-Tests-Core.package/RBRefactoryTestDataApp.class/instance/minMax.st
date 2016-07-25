lint
minMax
	"Bug in 3<5 ifTrue: [3]  ifFalse: [5]"

^3<5 ifTrue: [3]  ifFalse: [5]
"	| var |
	var := 4.
	var < 5 ifTrue: [var := 5].
	^var"
*Polymorph-Widgets
labelString
	"Answer the actual label string."

	^label isNil
		ifTrue: [labelString]
		ifFalse: [label contents asString]
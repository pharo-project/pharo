*Polymorph-Widgets
lastSubmorphRecursive
	"Answer recursive last submorph of the receiver."

	^self hasSubmorphs
		ifTrue: [self lastSubmorph lastSubmorphRecursive]
		ifFalse: [self]
copying
duplicate
	"Make and return a duplicate of the receiver"

	| newMorph topRend |
	((topRend := self topRendererOrSelf) ~~ self) ifTrue: [^ topRend duplicate].
	newMorph := self veryDeepCopy.
	newMorph arrangeToStartStepping.
		newMorph privateOwner: nil. "no longer in world"
	^newMorph
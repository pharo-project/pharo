*Morphic-Widgets-Windows
modelWakeUp
	"I am the model of a SystemWindow, that has just been activated"

	| aWindow |
	owner isNil ifTrue: [^self].	"Not in Morphic world"
	(owner isKindOf: TransformMorph) ifTrue: [^self viewBox: self fullBounds].
	(aWindow := self containingWindow) ifNotNil: 
			[self viewBox = aWindow panelRect 
				ifFalse: [self viewBox: aWindow panelRect]]
debug and other
buildDebugMenu: aHand
	"Answer a debugging menu for the receiver. 
	 The hand argument is seemingly historical and plays no role presently"

	| aMenu |
	aMenu := UIManager default newMenuIn: self for: self.
	(self hasProperty: #errorOnDraw)
		ifTrue: [ aMenu add: 'start drawing again' selector: #resumeAfterDrawError ].
	(self hasProperty: #drawError)
		ifTrue: [ aMenu add: 'debug drawing error' selector: #debugDrawError.
			aMenu addLine ].
	(self hasProperty: #errorOnStep)
		ifTrue: [ aMenu add: 'start stepping again' selector: #resumeAfterStepError.
			aMenu addLine ].
	aMenu add: 'inspect morph' selector: #inspectInMorphic:.
	aMenu lastItem
		icon: (self iconNamed: #smallInspectItIcon).
	aMenu add: 'inspect owner chain' selector: #inspectOwnerChain.
	aMenu lastItem
		icon: (self iconNamed: #smallInspectItIcon).

	aMenu addLine.
	aMenu
		add: 'browse morph class'
		target: self
		selector: #browseHierarchy.

	^ aMenu
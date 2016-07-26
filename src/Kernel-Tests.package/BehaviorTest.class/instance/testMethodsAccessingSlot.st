tests - queries
testMethodsAccessingSlot
	| numberViaSlot numberViaIVar |
	numberViaSlot := (Point methodsAccessingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsAccess: 'x') size.
	self assert: numberViaSlot = numberViaIVar.
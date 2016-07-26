tests - queries
testMethodsWritingSlot
	| numberViaSlot numberViaIVar |
	numberViaSlot := (Point methodsWritingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsAssign: 'x') size.
	self assert: numberViaSlot = numberViaIVar.
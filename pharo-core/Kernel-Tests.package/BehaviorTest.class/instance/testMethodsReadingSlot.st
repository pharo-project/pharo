tests - queries
testMethodsReadingSlot
	| numberViaSlot numberViaIVar |
	numberViaSlot := (Point methodsReadingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsRead: 'x') size.
	self assert: numberViaSlot = numberViaIVar.
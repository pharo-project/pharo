tests - queries
testMethodsWritingSlot
	| numberViaSlot numberViaIVar |
	
	"Check the source code availability to do not fail on images without sources"
	(Point>>#x) hasSourceCode ifFalse: [ ^ self ].

	numberViaSlot := (Point methodsWritingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsAssign: 'x') size.
	self assert: numberViaSlot = numberViaIVar.
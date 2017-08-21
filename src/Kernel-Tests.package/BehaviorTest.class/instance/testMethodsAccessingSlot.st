tests - queries
testMethodsAccessingSlot
	| numberViaSlot numberViaIVar |
	
	"Check the source code availability to do not fail on images without sources"
	(Point>>#x) hasSourceCode ifFalse: [ ^ self ].
	
	numberViaSlot := (Point methodsAccessingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsAccess: 'x') size.
	self assert: numberViaSlot = numberViaIVar.
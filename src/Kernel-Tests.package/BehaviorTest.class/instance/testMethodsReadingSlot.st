tests - queries
testMethodsReadingSlot
	| numberViaSlot numberViaIVar |
	
	"Check the source code availability to do not fail on images without sources"
	(Point>>#x) hasSourceCode ifFalse: [ ^ self ].
	
	numberViaSlot := (Point methodsReadingSlot: (Point slotNamed: #x)) size.
	numberViaIVar := (Point whichSelectorsRead: 'x') size.
	self assert: numberViaSlot = numberViaIVar.
tests - slots
testReadsSlot

	"Check the source code availability to do not fail on images without sources"
	({ Point>>#x. Point>>#setX:setY: } allSatisfy: #hasSourceCode) 
		ifFalse: [ ^ self ].

	self assert: ((Point>>#x) readsSlot: (Point slotNamed: #x)).
	self deny: ((Point>>#x) readsSlot: (Point slotNamed: #y)).
	self deny: ((Point>>#setX:setY:) readsSlot: (Point slotNamed: #y)).
	
	
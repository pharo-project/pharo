tests - slots
testWritesSlot

	"Check the source code availability to do not fail on images without sources"
	({ Point>>#x. Point>>#setX:setY: } allSatisfy: #hasSourceCode) 
		ifFalse: [ ^ self ].

	self deny: ((Point>>#x) writesSlot: (Point slotNamed: #x)).
	self deny: ((Point>>#x) writesSlot: (Point slotNamed: #y)).
	
	self assert: ((Point>>#setX:setY:) writesSlot: (Point slotNamed: #y)).
	self assert: ((Point>>#setX:setY:) writesSlot: (Point slotNamed: #x)).
	
tests - slots
testDefinesSlotNamed
	self assert: (Point definesSlotNamed: #x).
	self deny: (Point definesSlotNamed: #z).
	
	self assert: (Context definesSlotNamed: #stackp).
	self deny: (Context definesSlotNamed: #sender).
	
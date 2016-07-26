tests
testAdd
	entity add: 4.
	announcer announce: GLMTestAnnouncement.
	self assert: listMorph roots size equals: 4.
	self assert: listMorph model roots size equals: 4.
	self assert: treeMorph roots size equals: 4.
	self assert: treeMorph model roots size equals: 4.
	self assert: tableMorph roots size equals: 4.
	self assert: tableMorph model roots size equals: 4.
	self assert: labelMorph contents equals: '4'
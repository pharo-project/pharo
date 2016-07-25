tests
testRemove
	entity removeLast.
	announcer announce: GLMTestAnnouncement.
	self assert: listMorph model roots size equals: 2.
	self assert: treeMorph model roots size equals: 2.
	self assert: tableMorph model roots size equals: 2.
	self assert: labelMorph contents equals: '2'
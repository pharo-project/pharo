testing - into/outof world
testIntoWorldTransferToNewGuy
	| m1 m2 |
	"Create the guys"
	m1 := TestInWorldMorph new.
	m2 := TestInWorldMorph new.
	self assert: (m1 intoWorldCount = 0).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 0).
	self assert: (m2 outOfWorldCount = 0).

	morph addMorphFront: m1.
	m1 addMorphFront:  m2.
	self assert: (m1 intoWorldCount = 0).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 0).
	self assert: (m2 outOfWorldCount = 0).

	morph openInWorld.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 0).

	morph addMorphFront: m2.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 0).

	morph addMorphFront: m1.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 0).

	m2 addMorphFront: m1.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 0).

	morph delete.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 1).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 1).

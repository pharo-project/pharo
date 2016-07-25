testing - into/outof world
testIntoWorldCollapseOutOfWorld
	| m1 m2 collapsed |
	"Create the guys"
	m1 := TestInWorldMorph new.
	m2 := TestInWorldMorph new.
	self assert: (m1 intoWorldCount = 0).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 0).
	self assert: (m2 outOfWorldCount = 0).

	"add them to basic morph"
	morph addMorphFront: m1.
	m1 addMorphFront: m2.
	self assert: (m1 intoWorldCount = 0).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 0).
	self assert: (m2 outOfWorldCount = 0).

	"open the guy"
	morph openInWorld.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 0).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 0).

	"collapse it"
	collapsed := 	CollapsedMorph new beReplacementFor: morph.
	self assert: (m1 intoWorldCount = 1).
	self assert: (m1 outOfWorldCount = 1).
	self assert: (m2 intoWorldCount = 1).
	self assert: (m2 outOfWorldCount = 1).

	"expand it"
	collapsed collapseOrExpand.
	self assert: (m1 intoWorldCount = 2).
	self assert: (m1 outOfWorldCount = 1).
	self assert: (m2 intoWorldCount = 2).
	self assert: (m2 outOfWorldCount = 1).

	"delete it"
	morph delete.
	self assert: (m1 intoWorldCount = 2).
	self assert: (m1 outOfWorldCount = 2).
	self assert: (m2 intoWorldCount = 2).
	self assert: (m2 outOfWorldCount = 2).

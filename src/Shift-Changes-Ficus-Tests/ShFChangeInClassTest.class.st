Class {
	#name : #ShFChangeInClassTest,
	#superclass : #AbstractShiftChangesFicusTest,
	#category : 'Shift-Changes-Ficus-Tests'
}

{ #category : #tests }
ShFChangeInClassTest >> testChangeCategory [
	| old new categoryChange |
	old := self shFTestClassA.
	new := self shFTestClassB.
	results := old compareTo: new.

	categoryChange := results
		detect: [ :e | e class = ShFCategoryChange ]
		ifNone: [ self fail: 'It should have detected the change in the category' ].

	self assert: categoryChange oldValue equals: #'Shift-Changes-Ficus-Tests-TestClasses'.
	self assert: categoryChange newValue equals: #'Shift-Changes-Ficus-Tests-TestClassesB'
]

{ #category : #tests }
ShFChangeInClassTest >> testChangeCommentary [
	| old new categoryChange |
	old := self shFTestClassA.
	new := self shFTestClassAWithComment.
	results := old compareTo: new.

	categoryChange := results
		detect: [ :e | e class = ShFCommentChange ]
		ifNone: [ self fail: 'It should have detected the change in the comment' ].

	self assert: categoryChange oldValue equals: 'A Test Class'.
	self assert: categoryChange newValue equals: 'A Test Class with a different comment'
]

{ #category : #tests }
ShFChangeInClassTest >> testChangeInLayout [
	| old new categoryChange |
	old := self shFTestClassA.
	new := self shFTestClassWithByteLayout.
	results := old compareTo: new.

	categoryChange := results
		detect: [ :e | e class = ShFLayoutTypeChange ]
		ifNone: [ self fail: 'It should have detected the change in the layout' ].

	self assert: categoryChange oldValue equals: #FixedLayout.
	self assert: categoryChange newValue equals: #ByteLayout
]

{ #category : #tests }
ShFChangeInClassTest >> testChangeName [
	| old new changeNameDifference |
	old := self shFTestClassA.
	new := self shFTestClassB.
	results := old compareTo: new.
	changeNameDifference := results
		detect: [ :e | e class = ShFClassRename ]
		ifNone: [ self fail: 'It should have detected the change in the name' ].
	self assert: changeNameDifference oldValue equals: #ShFTestClassA.
	self assert: changeNameDifference newValue equals: #ShFTestClassB
]

{ #category : #tests }
ShFChangeInClassTest >> testChangeSuperclass [
	| old new change |
	old := self shFTestClassA.
	new := self shFTestClassC.
	results := old compareTo: new.
	change := results
		detect: [ :e | e class = ShFSuperclassChange ]
		ifNone: [ self fail: 'It should have detected the change' ].

	self assert: change oldValue equals: #Object.
	self assert: change newValue equals: #ShFTestClassB.
]

{ #category : #tests }
ShFChangeInClassTest >> testChangeTraitComposition [
	| old new categoryChange |
	old := self shFTestClassC.
	new := self shFTestClassCWithoutTrait.
	results := old compareTo: new.

	categoryChange := results
		detect: [ :e | e class = ShFTraitCompositionChange ]
		ifNone: [ self fail: 'It should have detected the change in the traitComposition ' ].

	self assert: categoryChange oldValue equals: 'TShFTestTrait'.
	self assert: categoryChange newValue equals: '{}'
]

{ #category : #tests }
ShFChangeInClassTest >> testClasspoolEntryAdded [
	| old new changes classpoolChange |
	old := self shFTestClassA.
	new := self shFTestClassAWithClassPoolEntry.
	results := old compareTo: new.

	changes := results select: [ :e | ShFClasspoolChange allSubclasses includes: e class ].
	
	self assert: changes size equals:1.

	classpoolChange := changes at:1.

	self assert: classpoolChange class equals: ShFClasspoolAdded.
	self assert: classpoolChange entryName equals: #AClassPoolEntry.

]

{ #category : #tests }
ShFChangeInClassTest >> testClasspoolEntryRemoved [
	| old new changes classpoolChange |
	old := self shFTestClassAWithClassPoolEntry.
	new := self shFTestClassA.
	results := old compareTo: new.

	changes := results select: [ :e | ShFClasspoolChange allSubclasses includes: e class ].
	
	self assert: changes size equals:1.

	classpoolChange := changes at:1.

	self assert: classpoolChange class equals: ShFClasspoolRemoved.
	self assert: classpoolChange entryName equals: #AClassPoolEntry.

]

{ #category : #tests }
ShFChangeInClassTest >> testNoChangeInLayout [
	| old new categoryChange |
	old := self shFTestClassA.
	new := self shFTestClassANew.
	results := old compareTo: new.

	categoryChange := results
		detect: [ :e | e class = ShFLayoutTypeChange ]
		ifFound: [ self fail: 'It should have not detected the change in the layout' ].

]

{ #category : #tests }
ShFChangeInClassTest >> testSharedPoolAdded [
	| old new changes sharedPoolChange |
	old := self shFTestClassA.
	new := self shFTestClassAWithSharedPool.
	results := old compareTo: new.

	changes := results select: [ :e | ShFSharedPoolChange allSubclasses includes: e class ].
	
	self assert: changes size equals:1.

	sharedPoolChange := changes at:1.

	self assert: sharedPoolChange class equals: ShFSharedPoolAdded.
	self assert: sharedPoolChange poolName equals: #ShFTestSharedPool.
	

]

{ #category : #tests }
ShFChangeInClassTest >> testSlotAdded [
	| old new changesInSlots slotChange |
	old := self shFTestClassA.
	new := self shFTestClassANew.
	results := old compareTo: new.

	changesInSlots := results select: [ :e | ShFSlotChange allSubclasses includes: e class ].
	
	self assert: changesInSlots size equals:1.

	slotChange := changesInSlots at:1.

	self assert: slotChange class equals: ShFSlotAdded.
	self assert: slotChange slotName equals: #otherSlot.
	self assert: slotChange slotType equals: #InstanceVariableSlot.
	self assert: slotChange slotSide equals: #instance. 
]

{ #category : #tests }
ShFChangeInClassTest >> testSlotRemoved [
	| old new changesInSlots slotChange |
	old := self shFTestClassANew.
	new := self shFTestClassA.
	results := old compareTo: new.

	changesInSlots := results select: [ :e | ShFSlotChange allSubclasses includes: e class ].
	
	self assert: changesInSlots size equals:1.

	slotChange := changesInSlots at:1.

	self assert: slotChange class equals: ShFSlotRemoved.
	self assert: slotChange slotName equals: #otherSlot.
	self assert: slotChange slotType equals: #InstanceVariableSlot.
	self assert: slotChange slotSide equals: #instance.
]

{ #category : #tests }
ShFChangeInClassTest >> testSlotTypeChanged [
	| old new changesInSlots slotChange |
	old := self shFTestClassWithDifferentSlotsOld.
	new := self shFTestClassWithDifferentSlotsNew.
	results := old compareTo: new.
	
	changesInSlots := results select: [ :e | ShFSlotChange allSubclasses includes: e class ].
	
	self assert: changesInSlots size equals:1.

	slotChange := changesInSlots at:1.

	self assert: slotChange class equals: ShFSlotTypeChanged .
	self assert: slotChange slotName equals: #oneSlot.
	self assert: slotChange oldSlotType equals: #InstanceVariableSlot.
	self assert: slotChange newSlotType equals: #PropertySlot.
	self assert: slotChange slotSide equals: #instance.

]

{ #category : #tests }
ShFChangeInClassTest >> testTwoSlotsAdded [
	| old new changesInSlots slotChange |
	old := self shFTestClassB.
	new := self shFTestClassANew.
	results := old compareTo: new.

	changesInSlots := results select: [ :e | ShFSlotChange allSubclasses includes: e class ].
	
	self assert: changesInSlots size equals:2.

	slotChange := changesInSlots at:1.

	self assert: slotChange class equals: ShFSlotAdded.
	self assert: slotChange slotName equals: #otherSlot.
	self assert: slotChange slotType equals: #InstanceVariableSlot.
	self assert: slotChange slotSide equals: #instance.
	
	slotChange := changesInSlots at:2.

	self assert: slotChange class equals: ShFSlotAdded.
	self assert: slotChange slotName equals: #oneSlot.
	self assert: slotChange slotType equals: #InstanceVariableSlot.
	self assert: slotChange slotSide equals: #instance.
]

{ #category : #tests }
ShFChangeInClassTest >> testTwoSlotsRemoved [
	| old new changesInSlots slotChange |
	old := self shFTestClassANew.
	new := self shFTestClassB.
	results := old compareTo: new.

	changesInSlots := results select: [ :e | ShFSlotChange allSubclasses includes: e class ].
	
	self assert: changesInSlots size equals:2.

	slotChange := changesInSlots at:1.

	self assert: slotChange class equals: ShFSlotRemoved.
	self assert: slotChange slotName equals: #otherSlot.
	self assert: slotChange slotType equals: #InstanceVariableSlot.
	self assert: slotChange slotSide equals: #instance.
	
	slotChange := changesInSlots at:2.

	self assert: slotChange class equals: ShFSlotRemoved.
	self assert: slotChange slotName equals: #oneSlot.
	self assert: slotChange slotType equals: #InstanceVariableSlot.
	self assert: slotChange slotSide equals: #instance.
]

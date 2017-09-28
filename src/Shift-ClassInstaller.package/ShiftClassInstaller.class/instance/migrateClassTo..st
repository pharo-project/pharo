migrating
migrateClassTo: newClass
	| slotsToMigrate |
	oldClass ifNil:[^ self].
	self assert: newClass isNotNil.
	
	oldClass 	superclass removeSubclass: oldClass.

	newClass subclasses: oldClass subclasses.

	slotsToMigrate := newClass class allSlots reject:[:e | Class hasSlotNamed: e name].
	slotsToMigrate do: [ :newSlot | oldClass class slotNamed: newSlot name ifFound: [ :oldSlot | newSlot write: (oldSlot read: oldClass) to: newClass ] ].
	
	oldClass classVarNames do: [ :aClassVar |
		(newClass hasClassVarNamed: aClassVar) ifTrue:[ newClass classVarNamed: aClassVar put: (oldClass classVarNamed: aClassVar) ].
	].
	 
	self migrateInstancesTo: newClass.
	
	oldClass becomeForward: newClass copyHash: false.
	builder oldMetaclass becomeForward: builder newMetaclass copyHash: false
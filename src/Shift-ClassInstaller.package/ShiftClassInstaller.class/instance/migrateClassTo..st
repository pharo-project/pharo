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
	
	[ 	
		(self builder changes anySatisfy: [ :e | e hasToMigrateInstances ]) 
			ifTrue: [self migrateInstancesTo: newClass].

		{ oldClass. builder oldMetaclass } elementsForwardIdentityTo: { newClass. builder newMetaclass }.	
	] valueUninterruptably.

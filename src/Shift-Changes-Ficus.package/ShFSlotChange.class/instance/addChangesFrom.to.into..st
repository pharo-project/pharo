checking change
addChangesFrom: oldDefinition to: newDefinition into: results
	self
		detectSlotChangeFrom: oldDefinition layout instanceVariables
		to: newDefinition layout instanceVariables
		into: results
		side: #instance.

	self
		detectSlotChangeFrom: oldDefinition metaclass layout instanceVariables
		to: newDefinition metaclass layout instanceVariables
		into: results
		side: #metaclass.


	results
		select: [ :e | ShSlotChange subclasses includes: e class ]
		thenDo: [ :e | 
			e
				oldClass: oldDefinition;
				newClass: newDefinition ]
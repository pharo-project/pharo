accessing
atName: aName
	^ self atName: aName ifAbsent: [ SlotNotFound signalForName: aName ].
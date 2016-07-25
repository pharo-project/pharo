accessing
serializedBy
	^ self dictionary at: #serializedBy ifAbsent: [ GTFuelPacking name ]
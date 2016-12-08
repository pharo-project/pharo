running
initializeSpecialObjectArray
"	objectSpace mustRebuildSpecialObjectsArray.
"	objectSpace specialObjectsArray: self newSpecialObjectsArray.

	self bootstrapInterpreter
		evaluateCode: 'Smalltalk specialObjectsArray: newArray.'
		withTemps: { 'newArray' -> objectSpace backend specialObjectsArray }.
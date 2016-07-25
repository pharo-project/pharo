instance creation
trackedClass: trackedClass forExtension: aCategory

	^ classes 
		at: trackedClass name, aCategory
		ifAbsentPut: [ self new 
			trackedClass: trackedClass;
			yourself ]
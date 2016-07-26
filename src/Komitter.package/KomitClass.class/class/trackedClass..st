instance creation
trackedClass: trackedClass

	^ classes 
		at: trackedClass name
		ifAbsentPut: [ self new 
			trackedClass: trackedClass;
			yourself ]
accessing
classRemoves

	^ changeRecords keys select:
		[:className | (changeRecords at: className) isClassRemoval]
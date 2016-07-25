instance creation
creationTimeFrom: aFileReference
	^ [ self creationTimeFromString: aFileReference basenameWithoutExtension ] 
		on: Error 
		do: [ aFileReference creationTime ]
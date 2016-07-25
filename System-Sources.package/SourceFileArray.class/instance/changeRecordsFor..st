public - string reading
changeRecordsFor: aMethodDefinition

	^ self 
		changeRecordsFrom: aMethodDefinition sourcePointer 
		className: aMethodDefinition methodClass theNonMetaClass name
		isMeta: aMethodDefinition methodClass isMeta
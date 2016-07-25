public - string reading
changeRecordsFor: aMethodDefinition do: aUnaryBlock
	"Evaluate aUnaryBlock with each of the ChangeRecords of aMethodDefinition. Most recent changes are evaluated first."

	^ self 
		changeRecordsFrom: aMethodDefinition sourcePointer
		className: aMethodDefinition theNonMetaClassName
		isMeta: aMethodDefinition isMetaSide
		do: aUnaryBlock
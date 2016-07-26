fileout
printSpaceTallyItem: aSpaceTallyItem percent: percent on: aStream
	aStream
		nextPutAll: (aSpaceTallyItem analyzedClassName padRightTo: 45);
		nextPutAll: (aSpaceTallyItem codeSize printString padLeftTo: 12);
		nextPutAll: (aSpaceTallyItem instanceCount printString padLeftTo: 12);
		nextPutAll: (aSpaceTallyItem spaceForInstances printString padLeftTo: 14).
	aStream 
		nextPutAll: ((percent printShowingDecimalPlaces: 2) padLeftTo: 12);
		nextPutAll: (((aSpaceTallyItem spaceForInstances / (aSpaceTallyItem instanceCount max: 1)) asFloat printShowingDecimalPlaces: 2) padLeftTo: 20);
		cr
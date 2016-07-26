printing
printOn: aStream

	super printOn: aStream.

	aStream 
		nextPut: $(;
		nextPutAll: self behaviorAffectedName;
		nextPutAll: '>>';
		nextPutAll: self methodAffectedSelector;
		nextPut: $)
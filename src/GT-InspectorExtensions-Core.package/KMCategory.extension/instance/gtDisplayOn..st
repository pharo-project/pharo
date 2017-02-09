*GT-InspectorExtensions-Core
gtDisplayOn: stream
	super gtDisplayOn: stream.
	^ stream
		nextPut: $(;
		nextPutAll: self name asString;
		nextPut: $)
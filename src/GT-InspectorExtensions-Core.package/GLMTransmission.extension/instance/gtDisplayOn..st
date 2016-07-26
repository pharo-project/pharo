*GT-InspectorExtensions-Core
gtDisplayOn: stream
	self origins asArray gtDisplayOn: stream.
	stream nextPutAll: '->'.
	self destination gtDisplayOn: stream
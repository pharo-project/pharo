*GT-InspectorExtensions-Core
gtDisplayOn: aStream
	aStream print: self methodClass; nextPutAll: '>>'; store: self selector.
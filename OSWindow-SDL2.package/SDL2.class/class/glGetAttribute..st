opengl
glGetAttribute: attr
	| value result |
   value := ExternalAddress allocate: 4.
	self glGetAttribute: attr into: value.
	result := value nbUInt32AtOffset: 0.
	value free.
	^result
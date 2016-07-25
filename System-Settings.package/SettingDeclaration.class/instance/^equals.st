comparing
= other
	self class = other class
		ifFalse: [^ false].
	self target == other target ifFalse: [^ false].
	self targetSelector = other targetSelector ifFalse: [^ false].
	self getSelector = other getSelector ifFalse: [^ false].
	self setSelector = other setSelector ifFalse: [^ false].
	^ true
	

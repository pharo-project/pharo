tests - simple
testCotrolInstead
	| link |
	link := MetaLink new 
	metaObject: [1+2];
	control: #instead.
	
	self assert: link metaObject value = 3.
	self assert: link selector = #value.
	self assert: link control = #instead.
	
	self deny: ((ReflectivityExamples>>#exampleMethod) ast hasMetalinkInstead). 	
	
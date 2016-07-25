tests - simple
testCotrolLink
	| link |
	link := MetaLink new 
	metaObject: [1+2];
	control: #after.
	
	self assert: link metaObject value = 3.
	self assert: link selector = #value.
	self assert: link control = #after.
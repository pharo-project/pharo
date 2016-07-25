tests - simple
testSimpleHaltLink
	| link |
	link := MetaLink new 
	metaObject: Halt;
	selector: #now.
	
	self assert: link metaObject = Halt.
	self assert: link selector = #now.
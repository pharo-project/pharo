method tests
testDefinesInstanceVariable
	self deny: (objectClass definesInstanceVariable: 'instanceVariable1').
	self assert: (newClass definesInstanceVariable: 'instanceVariable1').
	self deny: (messageNodeClass definesInstanceVariable: 'instanceVariable1').
	self assert: (messageNodeClass definesInstanceVariable: 'parent').
	self assert: (messageNodeClass definesInstanceVariable: 'selector')
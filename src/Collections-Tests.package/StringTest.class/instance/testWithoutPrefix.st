tests - element accessing
testWithoutPrefix
	self assert: ('UMLClass' withoutPrefix: 'UML') equals: 'Class'.
	self assert: ('UMLClass' withoutSuffix: 'Class') equals: 'UML'.
	
	self assert: ('UMLClass' withoutPrefix: 'Class') equals: 'UMLClass'.
	self assert: ('UMLClass' withoutSuffix: 'UML') equals: 'UMLClass'.
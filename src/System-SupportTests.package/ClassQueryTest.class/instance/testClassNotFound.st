tests
testClassNotFound
	
     self should: [ Smalltalk globals classNamed: #notExistedClass ] raise: ClassNotFound

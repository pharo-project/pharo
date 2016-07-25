tests
testAddInstVarNamedClassInterface

	"Add instance variables using the class interface"
	
	aClass := self make: [ :builder |
		builder name: self aClassName 
		].
	aClass addInstVarNamed: 'x'.

	self assert: aClass instVarNames = #(x).	


	anotherClass := self make: [ :builder |
		builder 
			superclass: aClass;
			name: self anotherClassName.
		].
	anotherClass addInstVarNamed: 'y'.
	
	self assert: anotherClass instVarNames = #(y).
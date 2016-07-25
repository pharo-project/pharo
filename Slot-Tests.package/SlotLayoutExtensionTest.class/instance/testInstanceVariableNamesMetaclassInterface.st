tests-invalid extensions
testInstanceVariableNamesMetaclassInterface
	"Add instance variables using the Metaclass interface"
	
	aClass := self make: [ :builder |
		builder name: self aClassName ].
	aClass class instanceVariableNames: 'x'.

	self assert: aClass class instVarNames equals: #(x).	

	anotherClass := self make: [ :builder |
		builder 
			superclass: aClass;
			name: self anotherClassName ].
	anotherClass class instanceVariableNames: 'y'.
	
	self assert: anotherClass class instVarNames equals: #(y).	
	self assert: (anotherClass class allInstVarNames includesAll: #(x y)).
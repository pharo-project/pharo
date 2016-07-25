tests
testAddInstVarNamed
	"Add instance variables using the builder interface"
	
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			slots: #( x )
		].

	self assert: aClass instVarNames = #(x).	


	anotherClass := self make: [ :builder |
		builder 
			superclass: aClass;
			name: self anotherClassName;
			slots: #( y )
		].
	
	self assert: anotherClass instVarNames = #(y).
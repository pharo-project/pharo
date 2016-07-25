tests
testReshapeByteVariableToPointerPropagatesToDeepHierarchy

	"create the original hierarchy"
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			superclass: Object;
			layoutClass: ByteLayout ]. 

	anotherClass := self make: [ :builder |
		builder 
			name:self anotherClassName;
			superclass: aClass;
			layoutClass: ByteLayout ].

	yetAnotherClass := self make: [ :builder |
		builder 
			name:self yetAnotherClassName;
			superclass: anotherClass;
			layoutClass: ByteLayout ].

	yetYetAnotherClass := self make: [ :builder |
		builder 
			name:self yetYetAnotherClassName;
			superclass: yetAnotherClass;
			layoutClass: ByteLayout ].

	"all the classes in the hierarchy should be of type variable byte"
	{ aClass. anotherClass. yetAnotherClass. yetYetAnotherClass } do: [ :each | 
		each classLayout checkIntegrity.
		self assert: each isVariable.
		self assert: each isBytes. ].
	
	"change the top superclass"	
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			superclass: Object;
			layoutClass: FixedLayout;
			slots: { #x } ]. 

	"all the classes in the hierarchy should be reshaped as fixed"
	{ aClass. anotherClass. yetAnotherClass. yetYetAnotherClass } do: [ :each | 
		each classLayout checkIntegrity.
		self assert: each isFixed description: each name, ' was not reshaped.'.
		self assert: each isPointers description: each name, ' was not reshaped.' ].
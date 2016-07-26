tests
testReshapePointerToByteVariablePropagatesToDeepHierarchy

	"create the original hierarchy"
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			superclass: Object;
			layoutClass: FixedLayout ]. 

	anotherClass := self make: [ :builder |
		builder 
			name:self anotherClassName;
			superclass: aClass;
			layoutClass: FixedLayout ].

	yetAnotherClass := self make: [ :builder |
		builder 
			name:self yetAnotherClassName;
			superclass: anotherClass;
			layoutClass: FixedLayout ].

	yetYetAnotherClass := self make: [ :builder |
		builder 
			name:self yetYetAnotherClassName;
			superclass: yetAnotherClass;
			layoutClass: FixedLayout ].

	"all the classes in the hierarchy should be of type fixed pointers"
	{ aClass. anotherClass. yetAnotherClass. yetYetAnotherClass } do: [ :each | 
		each classLayout checkIntegrity.
		self assert: each isFixed.
		self assert: each isPointers. ].
	
	"change the top superclass"	
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			superclass: Object;
			layoutClass: ByteLayout ]. 

	"all the classes in the hierarchy should be reshaped as fixed"
	{ aClass. anotherClass. yetAnotherClass. yetYetAnotherClass } do: [ :each | 
		each classLayout checkIntegrity.
		self assert: each isVariable description: each name, ' was not reshaped.'.
		self assert: each isBytes description: each name, ' was not reshaped.' ].
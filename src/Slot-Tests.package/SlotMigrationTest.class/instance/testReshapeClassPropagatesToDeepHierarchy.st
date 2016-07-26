tests
testReshapeClassPropagatesToDeepHierarchy
	"Test reshaping a class which is head of a hierarchy of 4 levels"
	
	"level 1"
	aClass := self make: [ :builder |
		builder name: self aClassName ].

	self assert: aClass subclasses isEmpty.
	aClass classLayout checkIntegrity.

	"level 2"
	anotherClass := self make: [ :builder |
		builder 
			superclass: aClass;
			name: self anotherClassName ].
	
	self assert: aClass subclasses equals: { anotherClass }.
	self assert: anotherClass subclasses isEmpty.
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.

	"level 3"
	yetAnotherClass := self make: [ :builder |
		builder 
			superclass: anotherClass;
			name: self yetAnotherClassName ].

	self assert: aClass subclasses equals: { anotherClass }.
	self assert: anotherClass subclasses equals: { yetAnotherClass }.
	self assert: yetAnotherClass subclasses isEmpty.
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.
	yetAnotherClass classLayout checkIntegrity.

	"level 4"
	yetYetAnotherClass := self make: [ :builder |
		builder 
			superclass: yetAnotherClass;
			name: self yetYetAnotherClassName ].


	self assert: aClass subclasses equals: { anotherClass }.
	self assert: anotherClass subclasses equals: { yetAnotherClass }.
	self assert: yetAnotherClass subclasses equals: { yetYetAnotherClass }.
	self assert: yetYetAnotherClass subclasses isEmpty.
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.
	yetAnotherClass classLayout checkIntegrity.
	yetYetAnotherClass classLayout checkIntegrity.

	"reshape level 1"
	aClass := self make: [ :builder |
		builder 
			name: self aClassName;
			slots:#(x) ].

	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.
	yetAnotherClass classLayout checkIntegrity.
	yetYetAnotherClass classLayout checkIntegrity.
tests
testReshapeClassPropagatesToDeepHierarchyClassInterface
	"Test reshaping a class which is head of a hierarchy of 4 levels"
	
	"level 1"
	aClass := Object
		subclass: self aClassName 
		instanceVariableNames: '' 
		classVariableNames: '' 
		poolDictionaries: '' 
		category: self aCategory.

	self assert: aClass subclasses isEmpty.
	aClass classLayout checkIntegrity.

	"level 2"
	anotherClass := aClass
		subclass: self anotherClassName 
		instanceVariableNames: '' 
		classVariableNames: '' 
		poolDictionaries: '' 
		category: self aCategory.
	
	self assert: aClass subclasses equals: { anotherClass }.
	self assert: anotherClass subclasses isEmpty.
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.

	"level 3"
	yetAnotherClass := anotherClass
		subclass: self yetAnotherClassName 
		instanceVariableNames: '' 
		classVariableNames: '' 
		poolDictionaries: '' 
		category: self aCategory.

	self assert: aClass subclasses equals: { anotherClass }.
	self assert: anotherClass subclasses equals: { yetAnotherClass }.
	self assert: yetAnotherClass subclasses isEmpty.
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.
	yetAnotherClass classLayout checkIntegrity.

	"level 4"
	yetYetAnotherClass := yetAnotherClass
		subclass: self yetYetAnotherClassName 
		instanceVariableNames: '' 
		classVariableNames: '' 
		poolDictionaries: '' 
		category: self aCategory.

	self assert: aClass subclasses equals: { anotherClass }.
	self assert: anotherClass subclasses equals: { yetAnotherClass }.
	self assert: yetAnotherClass subclasses equals: { yetYetAnotherClass }.
	self assert: yetYetAnotherClass subclasses isEmpty.
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.
	yetAnotherClass classLayout checkIntegrity.
	yetYetAnotherClass classLayout checkIntegrity.

	"reshape level 1"
	aClass := Object
		subclass: self aClassName 
		instanceVariableNames: 'x' 
		classVariableNames: '' 
		poolDictionaries: '' 
		category: self aCategory.

	self assert: aClass instVarNames equals: #(x).
	aClass classLayout checkIntegrity.
	anotherClass classLayout checkIntegrity.
	yetAnotherClass classLayout checkIntegrity.
	yetYetAnotherClass classLayout checkIntegrity.
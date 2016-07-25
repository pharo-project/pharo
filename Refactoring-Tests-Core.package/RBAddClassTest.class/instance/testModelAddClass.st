tests
testModelAddClass
	| refactoring newClass superClass subclass |
	subclass := model classNamed: #Bar.
	superClass := model classNamed: #Foo.
	refactoring := RBAddClassRefactoring 
		model: model
		addClass: #FooTest
		superclass: superClass
		subclasses: (Array with: subclass)
		category: #'Refactory-Testing'.
	self executeRefactoring: refactoring.
	newClass := model classNamed: #FooTest.
	self assert: newClass superclass = superClass.
	self assert: (superClass subclasses includes: newClass).
	self assert: newClass theMetaClass superclass = superClass theMetaClass.
	self assert: (superClass theMetaClass subclasses includes: newClass theMetaClass).
	self assert: subclass superclass = newClass.
	self assert: (newClass subclasses includes: subclass).
	self assert: subclass theMetaClass superclass = newClass theMetaClass.
	self assert: (newClass theMetaClass subclasses includes: subclass theMetaClass)
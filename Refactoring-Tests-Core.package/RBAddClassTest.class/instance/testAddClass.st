tests
testAddClass
	| refactoring newClass superClass classTest |
	refactoring := RBAddClassRefactoring 
		addClass: #FooTest
		superclass: RBRefactoringTest
		subclasses: (Array with: self class)
		category: #'Refactory-Testing'.
	self executeRefactoring: refactoring.
	newClass := refactoring model classNamed: #FooTest.
	superClass := refactoring model classNamed: #RBRefactoringTest.
	classTest := refactoring model classNamed: self class name.
	self assert: newClass superclass = superClass.
	self assert: (superClass subclasses includes: newClass).
	self assert: newClass theMetaClass superclass = superClass theMetaClass.
	self assert: (superClass theMetaClass subclasses includes: newClass theMetaClass).
	self assert: classTest superclass = newClass.
	self assert: (newClass subclasses includes: classTest).
	self assert: classTest theMetaClass superclass = newClass theMetaClass.
	self assert: (newClass theMetaClass subclasses includes: classTest theMetaClass)
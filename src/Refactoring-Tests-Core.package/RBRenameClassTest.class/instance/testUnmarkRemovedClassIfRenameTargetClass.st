tests
testUnmarkRemovedClassIfRenameTargetClass
	"Unmark a removed class if we rename another class to the removed class name.
rename class A to C (class A is marked as removed)
rename class B to A
-> class A is not marked as removed (model includesClassNamed:#A -> true"

	| addClass renameClass classA classB classC |
	classA := ('RBClass' , 'ToRename') asSymbol.
	classB := 'TestUnmarkClassRenameSource' asSymbol.
	classC := 'TestUnmarkClassRenameTarget' asSymbol.
	addClass := RBAddClassRefactoring
		model: model
		addClass: classB
		superclass: Object
		subclasses: {}
		category: self class package name.
	self executeRefactoring: addClass.
	self assert: (model includesClassNamed: classA).
	renameClass := RBRenameClassRefactoring new
		model: model;
		className: classA newName: classC;
		yourself.
	self executeRefactoring: renameClass.
	self deny: (model includesClassNamed: classA).
	self assert: (model includesClassNamed: classC).
	renameClass := RBRenameClassRefactoring new
		model: model;
		className: classB newName: classA;
		yourself.
	self executeRefactoring: renameClass.
	self deny: (model includesClassNamed: classB).
	self assert: (model includesClassNamed: classC).
	self assert: (model includesClassNamed: classA)
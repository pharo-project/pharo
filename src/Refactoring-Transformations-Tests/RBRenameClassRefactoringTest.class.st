Class {
	#name : 'RBRenameClassRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'failure tests' }
RBRenameClassRefactoringTest >> testFailureBadName [

	self shouldFail: (RBRenameClassRefactoring
			 rename: #RBLintRuleTestData
			 to: self objectClassVariable).
	self shouldFail:
		(RBRenameClassRefactoring rename: #RBLintRuleTestData to: #'Ob ject')
]

{ #category : 'failure tests' }
RBRenameClassRefactoringTest >> testFailureExistingName [

	self shouldFail:
		(RBRenameClassRefactoring rename: self class name to: #Object)
]

{ #category : 'failure tests' }
RBRenameClassRefactoringTest >> testFailureMetaClassFailure [

	self shouldFail:
		(RBRenameClassRefactoring rename: self class class name to: #Foo)
]

{ #category : 'tests' }
RBRenameClassRefactoringTest >> testModelRenameSequenceClass [

	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #Foo1;
			package: #'Refactory-Test data' ].
	(model classNamed: #Foo1)
		compile: 'foo	 ^ Foo1' classified: #( accessing );
		compile: 'objectName ^ #(Foo1)' classified: #( accessing ).


	(RBRenameClassRefactoring model: model rename: #Foo1 to: #Foo2) generateChanges.
	(RBRenameClassRefactoring model: model rename: #Foo2 to: #Foo3) generateChanges.

	self deny: (model includesClassNamed: #Foo1).
	self deny: (model includesClassNamed: #Foo2).
	self assert: (model includesClassNamed: #Foo3).
	self assert: ((model classNamed: #Foo3) parseTreeForSelector: #foo) equals: (self parseMethod: 'foo ^ Foo3').
	self assert: ((model classNamed: #Foo3) parseTreeForSelector: #objectName) equals: (self parseMethod: 'objectName ^ #(Foo3)')
]

{ #category : 'tests' }
RBRenameClassRefactoringTest >> testRefactoring [

	| refactoring class |
	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #Foo;
			slots: { #a };
			package: #'Refactoring-Refactorings-Tests' ].

	class := model classNamed: #Foo.
	class
		compile: 'foo	^Object' classified: #( #accessing );
		compile: 'objectName ^#(Object)' classified: #( #accessing ).

	refactoring := RBRenameClassRefactoring model: model rename: #Object to: #Thing.

	refactoring generateChanges.

	self assert: (model includesClassNamed: #Thing).
	self deny: (model includesClassNamed: #Object).
	self assert: (class parseTreeForSelector: #foo) equals: (self parseMethod: 'foo ^Thing').
	self assert: (class parseTreeForSelector: #objectName) equals: (self parseMethod: 'objectName ^#(Thing)').
	self assert: class superclass name equals: #Thing
]

{ #category : 'tests' }
RBRenameClassRefactoringTest >> testRenameClass [
	| refactoring class classA classB classC aModel |

	classA := ('RBClass', 'ToRename') asSymbol.
	classB := ('RBNew', 'ClassName') asSymbol.
	classC := #RBSubclassOfClassToRename.

	refactoring := RBRenameClassRefactoring rename: classA to: classB.
	refactoring generateChanges.

	aModel := refactoring model.
	self assert: ((aModel classNamed: classB) parseTreeForSelector: #method1)
		  		equals: (self parseMethod: 'method1
	^self method2').
	self deny: (aModel includesClassNamed: classA).
	class := aModel classNamed: classC.
	self assert: class superclass equals: (aModel classNamed: classB).
	self assert: (class parseTreeForSelector: #symbolReference)
				equals: (self parseMethod: 'symbolReference
								^#RBNewClassName').
	self assert: (class parseTreeForSelector: #reference)
				equals: (self parseMethod: 'reference
								^RBNewClassName new')
]

{ #category : 'tests' }
RBRenameClassRefactoringTest >> testRenameClassFromTrait [
	| refactoring class classA classB classC aModel |

	classA := ('RBClass', 'ToRename') asSymbol.
	classB := ('RBNew', 'ClassName') asSymbol.
	classC := #RBTDummy.
	refactoring := RBRenameClassRefactoring rename: classA to: classB.
	refactoring generateChanges.
	aModel := refactoring model.
	self assert: (aModel includesClassNamed: classB).
	self deny: (aModel includesClassNamed: classA).
	class := aModel classNamed: classC.
	self assert: (class parseTreeForSelector: #methodFromTrait)
				equals: (self parseMethod: 'methodFromTrait
	RBNewClassName justForTest').
	self assert: ((refactoring model classNamed: #RBClassUsingSharedPoolForTestData) methodFor: #methodFromTrait) modelClass name equals: #RBTDummy
]

{ #category : 'tests' }
RBRenameClassRefactoringTest >> testUnmarkRemovedClassIfRenameTargetClass [
	"Unmark a removed class if we rename another class to the removed class name.
rename class A to C (class A is marked as removed)
rename class B to A
-> class A is not marked as removed (model includesClassNamed:#A -> true"

	| addClass renameClass classA classB classC |
	classA := ('RBClass' , 'ToRename') asSymbol.
	classB := 'TestUnmarkClassRenameSource' asSymbol.
	classC := 'TestUnmarkClassRenameTarget' asSymbol.
	addClass := (RBInsertNewClassTransformation model: model className: classB)
		            superclass: #Object;
		            packageName: self class package name;
		            yourself.
	addClass generateChanges.
	self assert: (model includesClassNamed: classA).
	renameClass := RBRenameClassRefactoring model: model rename: classA to: classC.
	renameClass generateChanges.
	self deny: (model includesClassNamed: classA).
	self assert: (model includesClassNamed: classC).
	renameClass := RBRenameClassRefactoring model: model rename: classB to: classA.
	renameClass generateChanges.
	self deny: (model includesClassNamed: classB).
	self assert: (model includesClassNamed: classC).
	self assert: (model includesClassNamed: classA)
]

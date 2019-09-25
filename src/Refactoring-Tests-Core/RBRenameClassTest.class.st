Class {
	#name : #RBRenameClassTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #'failure tests' }
RBRenameClassTest >> testBadName [
	self
		shouldFail: (RBRenameClassRefactoring 
				rename: RBLintRuleTestData
				to: self objectClassVariable);
		shouldFail: (RBRenameClassRefactoring 
				rename: RBLintRuleTestData
				to: #'Ob ject')
]

{ #category : #'failure tests' }
RBRenameClassTest >> testExistingName [
	self shouldFail: (RBRenameClassRefactoring rename: self class to: #Object)
]

{ #category : #'failure tests' }
RBRenameClassTest >> testMetaClassFailure [
	self 
		shouldFail: (RBRenameClassRefactoring rename: self class class to: #Foo)
]

{ #category : #tests }
RBRenameClassTest >> testModelRenameClass [
	| refactoring class |
	model 
		defineClass: 'Object subclass: #Foo instanceVariableNames: ''a'' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	class := model classNamed: #Foo.
	class
		compile: 'foo	^Object' classified: #(#accessing);
		compile: 'objectName ^#(Object)' classified: #(#accessing).
	refactoring := RBRenameClassRefactoring 
				model: model
				rename: Object
				to: #Thing.
	self executeRefactoring: refactoring.
	self assert: (model includesClassNamed: #Thing).
	self deny: (model includesClassNamed: #Object).
	self 
		assert: (class parseTreeFor: #foo) equals:  (self parseMethod: 'foo ^Thing').
	self assert: (class parseTreeFor: #objectName) 
				equals: (self parseMethod: 'objectName ^#(Thing)').
	self assert: class superclass name equals: #Thing
]

{ #category : #tests }
RBRenameClassTest >> testModelRenameSequenceClass [
	model 
		defineClass: 'Object subclass: #Foo1 
			instanceVariableNames: '''' 
			classVariableNames: '''' poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	(model classNamed: #Foo1)
		compile: 'foo	 ^ Foo1' classified: #(accessing);
		compile: 'objectName ^ #(Foo1)' classified: #(accessing).
	self executeRefactoring: (RBRenameClassRefactoring 
		model: model
		rename: (model classNamed: #Foo1)
		to: #Foo2).
	self executeRefactoring: (RBRenameClassRefactoring 
		model: model
		rename: (model classNamed: #Foo2)
		to: #Foo3).
	self deny: (model includesClassNamed: #Foo1).
	self deny: (model includesClassNamed: #Foo2).
	self assert: (model includesClassNamed: #Foo3).
	self assert: ((model classNamed: #Foo3) parseTreeFor: #foo) 
		equals: (self parseMethod: 'foo ^ Foo3').
	self assert: ((model classNamed: #Foo3) parseTreeFor: #objectName) 
		equals: (self parseMethod: 'objectName ^ #(Foo3)')
]

{ #category : #tests }
RBRenameClassTest >> testRenameClass [
	| refactoring class |
	refactoring := RBRenameClassRefactoring 
				rename: (testingEnvironment at: ('RBClass' , 'ToRename') asSymbol)
				to: 'RBNew' , 'ClassName' asSymbol.
	self executeRefactoring: refactoring.
	self 
		assert: ((refactoring model classNamed: ('RBNew' , 'ClassName') asSymbol) 
				parseTreeFor: #method1) equals: (self parseMethod: 'method1
	^self method2').
	self deny: (refactoring model 
				includesClassNamed: ('RBClass' , 'ToRename') asSymbol).
	class := refactoring model 
				classNamed: ('RBSubclass' , 'OfClassToRename') asSymbol.
	self assert: class superclass 
				equals: (refactoring model classNamed: ('RBNew' , 'ClassName') asSymbol).
	self assert: (class parseTreeFor: #symbolReference) 
				equals: (self parseMethod: 'symbolReference
								^#RBNewClassName').
	self assert: (class parseTreeFor: #reference) 
				equals: (self parseMethod: 'reference
								^RBNewClassName new')
]

{ #category : #tests }
RBRenameClassTest >> testUnmarkRemovedClassIfRenameTargetClass [
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
]

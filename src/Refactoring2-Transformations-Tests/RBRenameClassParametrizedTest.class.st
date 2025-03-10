Class {
	#name : #RBRenameClassParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #tests }
RBRenameClassParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBRenameClassRefactoring };
		addCase: { #rbClass -> RBRenameClassTransformation };
		yourself
]

{ #category : #accessing }
RBRenameClassParametrizedTest >> constructor [
	^ #rename:to:
]

{ #category : #'failure tests' }
RBRenameClassParametrizedTest >> testBadName [
	self shouldFail: (self createRefactoringWithArguments:
		{ #RBLintRuleTestData . self objectClassVariable }).
	self shouldFail: (self createRefactoringWithArguments:
		{ #RBLintRuleTestData . #'Ob ject' })
]

{ #category : #'failure tests' }
RBRenameClassParametrizedTest >> testExistingName [
	self shouldFail:
		(self createRefactoringWithArguments:  { self class name . #Object })
]

{ #category : #'failure tests' }
RBRenameClassParametrizedTest >> testMetaClassFailure [

	self shouldFail:
		(self createRefactoringWithArguments: { self class class name . #Foo })
]

{ #category : #tests }
RBRenameClassParametrizedTest >> testModelRenameSequenceClass [

	model defineClass: 'Object << #Foo1
			package: #''Refactory-Test data'''.
	(model classNamed: #Foo1)
		compile: 'foo	 ^ Foo1' classified: #(accessing);
		compile: 'objectName ^ #(Foo1)' classified: #(accessing).

	self executeRefactoring: (self createRefactoringWithModel: model
		andArguments: { #Foo1 . #Foo2 }).
	self executeRefactoring: (self createRefactoringWithModel: model
		andArguments: { #Foo2 . #Foo3 }).

	self deny: (model includesClassNamed: #Foo1).
	self deny: (model includesClassNamed: #Foo2).
	self assert: (model includesClassNamed: #Foo3).
	self assert: ((model classNamed: #Foo3) parseTreeForSelector: #foo)
		  equals: (self parseMethod: 'foo ^ Foo3').
	self assert: ((model classNamed: #Foo3) parseTreeForSelector: #objectName)
		  equals: (self parseMethod: 'objectName ^ #(Foo3)')
]

{ #category : #tests }
RBRenameClassParametrizedTest >> testRefactoring [

	| refactoring class |
	model 	defineClass: 'Object << #Foo
		slots: { #a };
		package: #''Refactoring2-Refactorings-Tests'''.

	class := model classNamed: #Foo.
	class compile: 'foo	^Object' classified: #(#accessing);
			compile: 'objectName ^#(#Object)' classified: #(#accessing).

	refactoring := self createRefactoringWithModel: model andArguments: { #Object . #Thing }.

	self executeRefactoring: refactoring.

	self assert: (model includesClassNamed: #Thing).
	self deny: (model includesClassNamed: #Object).
	self assert: (class parseTreeForSelector: #foo)
		  equals: (self parseMethod: 'foo ^Thing').
	self assert: (class parseTreeForSelector: #objectName)
		  equals: (self parseMethod: 'objectName ^#(Thing)').
	self assert: class superclass name equals: #Thing
]

{ #category : #tests }
RBRenameClassParametrizedTest >> testRenameClass [
	| refactoring class classA classB classC aModel |

	classA := ('RBClass', 'ToRename') asSymbol.
	classB := ('RBNew', 'ClassName') asSymbol.
	classC := #RBSubclassOfClassToRename.

	refactoring := self createRefactoringWithArguments: { classA . classB }.
	self executeRefactoring: refactoring.

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

{ #category : #tests }
RBRenameClassParametrizedTest >> testRenameClassFromTrait [
	| refactoring class classA classB classC aModel |

	classA := ('RBClass', 'ToRename') asSymbol.
	classB := ('RBNew', 'ClassName') asSymbol.
	classC := #RBDummy.
	refactoring := self createRefactoringWithArguments: { classA . classB }.
	self executeRefactoring: refactoring.
	aModel := refactoring model.
	self assert: (aModel includesClassNamed: classB).
	self deny: (aModel includesClassNamed: classA).
	class := aModel classNamed: classC.
	self assert: (class parseTreeForSelector: #methodFromTrait)
				equals: (self parseMethod: 'methodFromTrait
	RBNewClassName justForTest').
	self assert: ((refactoring model classNamed: #RBClassUsingSharedPoolForTestData) methodFor: #methodFromTrait) modelClass name equals: #RBDummy
]

{ #category : #tests }
RBRenameClassParametrizedTest >> testUnmarkRemovedClassIfRenameTargetClass [
	"Unmark a removed class if we rename another class to the removed class name.
rename class A to C (class A is marked as removed)
rename class B to A
-> class A is not marked as removed (model includesClassNamed:#A -> true"

	| addClass renameClass classA classB classC |

	classA := ('RBClass' , 'ToRename') asSymbol.
	classB := 'TestUnmarkClassRenameSource' asSymbol.
	classC := 'TestUnmarkClassRenameTarget' asSymbol.
	addClass := RBInsertClassTransformation
		model: model
		addClass: classB
		superclass: #Object
		subclasses: {}
		category: self class package name.
	self executeRefactoring: addClass asRefactoring.
	self assert: (model includesClassNamed: classA).
	renameClass := self createRefactoringWithModel: model andArguments: { classA . classC }.
	self executeRefactoring: renameClass asRefactoring.
	self deny: (model includesClassNamed: classA).
	self assert: (model includesClassNamed: classC).
	renameClass :=self createRefactoringWithModel: model andArguments: { classB . classA }.
	self executeRefactoring: renameClass asRefactoring.
	self deny: (model includesClassNamed: classB).
	self assert: (model includesClassNamed: classC).
	self assert: (model includesClassNamed: classA)
]

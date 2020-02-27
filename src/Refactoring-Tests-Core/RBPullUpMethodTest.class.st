Class {
	#name : #RBPullUpMethodTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #tests }
RBPullUpMethodTest >> testPullUpAndCopyDown [
	| class |
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Subclass subclass: #Foo1 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Subclass subclass: #Foo2 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	class := model classNamed: #Foo1.
	class compile: 'yourself ^1' classified: #(#accessing).
	self proceedThroughWarning: [ self executeRefactoring: (RBPullUpMethodRefactoring 
				model: model
				pullUp: #(#yourself)
				from: class) ].
	self assert: (class superclass parseTreeFor: #yourself) 
			equals: (self parseMethod: 'yourself ^1').
	self deny: (class directlyDefinesMethod: #yourself).
	class := model classNamed: #Foo2.
	self assert: (class directlyDefinesMethod: #yourself).
	self assert: (class parseTreeFor: #yourself) 
			equals: ((model classNamed: #Object) parseTreeFor: #yourself)
]

{ #category : #'failure tests' }
RBPullUpMethodTest >> testPullUpClassMethod [
	| class |
	class := model classNamed: #Object.
	class addClassVariable: #Foo.
	class classSide 
		compile: 'foo ^Foo'
		classified: #(#accessing ).
	self shouldFail: (RBPullUpMethodRefactoring 
			model: model
			pullUp: #(#foo )
			from: class classSide)
]

{ #category : #tests }
RBPullUpMethodTest >> testPullUpInAHighHierarchyClass [
	| class superClass |
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Subclass subclass: #Foo1 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	class := model classNamed: #Foo1.
	superClass := model classNamed: #SomeClass .
	class compile: 'example ^1' classified: #(#accessing).
	self executeRefactoring: (RBPullUpMethodRefactoring 
				model: model
				pullUp: #(#example)
				from: class 
				to: superClass ).
	self assert: (superClass parseTreeFor: #example) 
				equals: (self parseMethod: 'example ^1').
	self deny: (class directlyDefinesMethod: #example).
	class := model classNamed: #Subclass.
	self assert: (class definesMethod: #example).
	self assert: (class parseTreeFor: #example) 
				equals: ((model classNamed: #SomeClass) parseTreeFor: #example)
]

{ #category : #tests }
RBPullUpMethodTest >> testPullUpMethodWithCopyOverriddenMethodsDown [
	| refactoring |
	self proceedThroughWarning: 
		[ refactoring := RBPullUpMethodRefactoring 
			pullUp: #(#isComposite )
			from: RBCompositeLintRuleTestData.
		self executeRefactoring: refactoring ].
	self assert: ((refactoring model classNamed: #RBBasicLintRuleTestData) parseTreeFor: #isComposite) equals: (self parseMethod: 'isComposite ^false').
	self assert: ((refactoring model classNamed: ('RBFoo' , 'LintRuleTestData') asSymbol) parseTreeFor: #isComposite) equals: (self parseMethod: 'isComposite ^false').
	self assert: ((refactoring model classNamed: #RBLintRuleTestData) parseTreeFor: #isComposite) equals: (self parseMethod: 'isComposite ^true').
	self deny: ((refactoring model classNamed: #RBCompositeLintRuleTestData) directlyDefinesMethod: #isComposite)
]

{ #category : #'failure tests' }
RBPullUpMethodTest >> testPullUpReferencesInstVar [
	self shouldFail: (RBPullUpMethodRefactoring 
			pullUp: #(#checkClass: )
			from: RBBasicLintRuleTestData)
]

{ #category : #'failure tests' }
RBPullUpMethodTest >> testPullUpWhenSuperClassDoesNotDirectlyImplement [
	| classEnvironment classes |
	classes := #(#ClassA #ClassB #ClassC)
		inject: OrderedCollection new
		into: [ :sum :each | 
			testingEnvironment
				at: each
				ifPresent: [ :class | 
					sum
						add: class;
						add: class class ].
			sum ].
	classEnvironment := RBClassEnvironment classes: classes.
	model name: 'Test'.
	"Classes"
	#('Object subclass: #ClassA
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''Testing'' ' 'ClassA subclass: #ClassB
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''Testing'' ' 'ClassB subclass: #ClassC
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''Testing'' ') do: [ :each | model defineClass: each ].
	#(#(#ClassA #(#('foo
			^ ''ClassA foo''' #private))) #(#ClassC #(#('foo
			^ ''ClassC foo''' #private))))
		do: [ :each | 
			| class |
			class := model classNamed: each first.
			each last
				do:
					[ :methodPair | class compile: methodPair first classified: methodPair last ] ].
	self
		shouldntWarn:
			(RBPullUpMethodRefactoring
				model: model
				pullUp: #(#foo)
				from: (model classNamed: #ClassC))
]

{ #category : #'failure tests' }
RBPullUpMethodTest >> testPullUpWithInvalidSuperSend [
	| class |
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Subclass subclass: #Foo1 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Subclass subclass: #Foo2 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #Foo2) compile: 'yourself ^super yourself + 1'
		classified: #(#accessing).
	class := model classNamed: #Foo1.
	class compile: 'yourself ^1' classified: #(#accessing).
	self shouldFail: (RBPullUpMethodRefactoring 
				model: model
				pullUp: #(#yourself)
				from: class)
]

{ #category : #'failure tests' }
RBPullUpMethodTest >> testPullUpWithMethodThatCannotBePushedDown [
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #SomeClass) compile: 'yourself ^54'
		classified: #(#accessing).
	self shouldFail: (RBPullUpMethodRefactoring 
				model: model
				pullUp: #(#yourself)
				from: (model classNamed: #SomeClass))
]

{ #category : #'failure tests' }
RBPullUpMethodTest >> testPullUpWithSuperSendThatCannotBeCopiedDown [
	| class |
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #Object) compile: 'foo ^3' classified: #(#accessing).
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #Subclass) compile: 'foo ^super foo'
		classified: #(#accessing).
	model 
		defineClass: 'Subclass subclass: #Foo1 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'Subclass subclass: #Foo2 instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(class := model classNamed: #Foo2) compile: 'foo ^1'
		classified: #(#accessing).
	self shouldFail: (RBPullUpMethodRefactoring 
				model: model
				pullUp: #(#foo)
				from: class)
]

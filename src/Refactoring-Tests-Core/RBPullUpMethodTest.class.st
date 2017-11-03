Class {
	#name : #RBPullUpMethodTest,
	#superclass : #RBRefactoringTest,
	#category : #Refactoring-Tests-Core
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
	self executeRefactoring: (RBPullUpMethodRefactoring 
				model: model
				pullUp: #(#yourself)
				from: class).
	self assert: (class superclass parseTreeFor: #yourself) 
				= (RBParser parseMethod: 'yourself ^1').
	self deny: (class directlyDefinesMethod: #yourself).
	class := model classNamed: #Foo2.
	self assert: (class directlyDefinesMethod: #yourself).
	self assert: (class parseTreeFor: #yourself) 
				= ((model classNamed: #Object) parseTreeFor: #yourself)
]

{ #category : #'failure tests' }
RBPullUpMethodTest >> testPullUpClassMethod [
	| class |
	class := model classNamed: #Object.
	class addClassVariable: #Foo.
	class theMetaClass 
		compile: 'foo ^Foo'
		classified: #(#accessing ).
	self shouldFail: (RBPullUpMethodRefactoring 
			model: model
			pullUp: #(#foo )
			from: class theMetaClass)
]

{ #category : #tests }
RBPullUpMethodTest >> testPullUpMethodWithCopyOverriddenMethodsDown [
	| refactoring |
	self proceedThroughWarning: 
		[ refactoring := RBPullUpMethodRefactoring 
			pullUp: #(#isComposite )
			from: RBCompositeLintRuleTest.
		self executeRefactoring: refactoring ].
	self assert: ((refactoring model classNamed: #RBBasicLintRuleTest) parseTreeFor: #isComposite) = (RBParser parseMethod: 'isComposite ^false').
	self assert: ((refactoring model classNamed: ('RBFoo' , 'LintRuleTest') asSymbol) parseTreeFor: #isComposite) = (RBParser parseMethod: 'isComposite ^false').
	self assert: ((refactoring model classNamed: #RBLintRuleTest) parseTreeFor: #isComposite) = (RBParser parseMethod: 'isComposite ^true').
	self deny: ((refactoring model classNamed: #RBCompositeLintRuleTest) directlyDefinesMethod: #isComposite)
]

{ #category : #'failure tests' }
RBPullUpMethodTest >> testPullUpReferencesInstVar [
	self shouldFail: (RBPullUpMethodRefactoring 
			pullUp: #(#checkClass: )
			from: RBBasicLintRuleTest)
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

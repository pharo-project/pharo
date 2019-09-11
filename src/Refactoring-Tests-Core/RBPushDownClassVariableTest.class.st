Class {
	#name : #RBPushDownClassVariableTest,
	#superclass : #RBRefactoringTest,
	#category : #'Refactoring-Tests-Core'
}

{ #category : #'failure tests' }
RBPushDownClassVariableTest >> testModelNonExistantName [
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	self shouldFail: (RBPushDownClassVariableRefactoring 
				model: model
				variable: #Foo
				class: (model classNamed: #SomeClass))
]

{ #category : #'failure tests' }
RBPushDownClassVariableTest >> testModelPushDownToMultipleSubclassesFailure [
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model classNamed: #Subclass) compile: 'foo ^Foo' classified: #(#accessing).
	model 
		defineClass: 'SomeClass subclass: #AnotherSubclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model metaclassNamed: #AnotherSubclass) compile: 'bar ^Foo'
		classified: #(#accessing).
	self shouldFail: (RBPushDownClassVariableRefactoring 
				model: model
				variable: #Foo
				class: (model classNamed: #SomeClass))
]

{ #category : #tests }
RBPushDownClassVariableTest >> testModelPushDownVariable [
	| class |
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	class := model classNamed: #Subclass.
	class compile: 'foo ^Foo' classified: #(#accessing).
	self executeRefactoring: (RBPushDownClassVariableRefactoring 
				model: model
				variable: #Foo
				class: class superclass).
	self deny: (class superclass directlyDefinesClassVariable: #Foo).
	self assert: (class directlyDefinesClassVariable: #Foo)
]

{ #category : #tests }
RBPushDownClassVariableTest >> testModelPushDownVariableToClassDownTwoLevels [
	| class |
	model defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model defineClass: 'Subclass subclass: #AnotherSubclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	class := model metaclassNamed: #AnotherSubclass.
	class 
		compile: 'bar ^Foo'
		classified: #(#accessing ).
	class := class instanceSide.
	self executeRefactoring: (RBPushDownClassVariableRefactoring 
			model: model
			variable: #Foo
			class: class superclass superclass).
	self deny: (class superclass superclass directlyDefinesClassVariable: #Foo).
	self deny: (class superclass directlyDefinesClassVariable: #Foo).
	self assert: (class directlyDefinesClassVariable: #Foo)
]

{ #category : #tests }
RBPushDownClassVariableTest >> testModelPushDownVariableToMultipleClassesInSameHierarchy [
	| class |
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	class := model classNamed: #Subclass.
	class compile: 'foo ^Foo' classified: #(#accessing).
	model 
		defineClass: 'Subclass subclass: #AnotherSubclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	(model metaclassNamed: #AnotherSubclass) compile: 'bar ^Foo'
		classified: #(#accessing).
	self executeRefactoring: (RBPushDownClassVariableRefactoring 
				model: model
				variable: #Foo
				class: class superclass).
	self deny: (class superclass directlyDefinesClassVariable: #Foo).
	self assert: (class directlyDefinesClassVariable: #Foo)
]

{ #category : #tests }
RBPushDownClassVariableTest >> testModelRemoveUnusedVariable [
	model 
		defineClass: 'Object subclass: #SomeClass instanceVariableNames: '''' classVariableNames: ''Foo'' poolDictionaries: '''' category: #''Refactory-Test data'''.
	self 
		assert: ((model classNamed: #SomeClass) directlyDefinesVariable: #Foo).
	model 
		defineClass: 'SomeClass subclass: #Subclass instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: #''Refactory-Test data'''.
	self executeRefactoring: (RBPushDownClassVariableRefactoring 
				model: model
				variable: #Foo
				class: (model classNamed: #SomeClass)).
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: #Foo).
	self deny: ((model classNamed: #Subclass) directlyDefinesVariable: #Foo)
]

{ #category : #'failure tests' }
RBPushDownClassVariableTest >> testNonExistantName [
	self shouldFail: (RBPushDownClassVariableRefactoring 
			variable: #Foo
			class: RBBasicLintRuleTestData)
]

{ #category : #tests }
RBPushDownClassVariableTest >> testPushDownClassVariable [
	| refactoring |
	refactoring := RBPushDownClassVariableRefactoring 
		variable: #Foo1
		class: RBLintRuleTestData.
	self assert: ((refactoring model classNamed: #RBLintRuleTestData) directlyDefinesClassVariable: #Foo1).
	self executeRefactoring: refactoring.
	(refactoring model classNamed: #RBLintRuleTestData) withAllSubclasses do: [ :each | self deny: (each directlyDefinesClassVariable: #Foo1) ]
]

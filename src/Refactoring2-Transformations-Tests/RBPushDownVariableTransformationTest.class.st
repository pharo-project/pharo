Class {
	#name : #RBPushDownVariableTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBPushDownVariableTransformationTest >> testClassVariable [

	| refactoring class |
	refactoring := (RBPushDownVariableTransformation
						classVariable: #Foo1
						class: #RBDummyLintRuleTest)
						asRefactoring.
	
	class := (refactoring model classNamed: #RBDummyLintRuleTest).
	self assert: (class directlyDefinesClassVariable: #Foo1).
	
	refactoring transform.
	class withAllSubclasses do: [ :each | self deny: (each directlyDefinesClassVariable: #Foo1) ]
]

{ #category : #testing }
RBPushDownVariableTransformationTest >> testClassVariableToMultipleSubclasses [

	| class |
	model 	defineClass: 'Object subclass: #SomeClass
			instanceVariableNames: ''''
			classVariableNames: ''Foo''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	model 	defineClass: 'SomeClass subclass: #Subclass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	class := model classNamed: #Subclass.
	class compile: 'foo ^Foo' classified: #(#accessing).
	model 	defineClass: 'Subclass subclass: #AnotherSubclass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	(model metaclassNamed: #AnotherSubclass) compile: 'bar ^Foo' classified: #(#accessing).
		
	(RBPushDownVariableTransformation
	 	model: model
	 	classVariable: #Foo
	 	class: #SomeClass)
	 	asRefactoring transform.
				
	self deny: (class superclass directlyDefinesClassVariable: #Foo).
	self assert: (class directlyDefinesClassVariable: #Foo)
]

{ #category : #testing }
RBPushDownVariableTransformationTest >> testClassVariableToMultipleSubclasses2 [

	| class |
	model defineClass: 'Object subclass: #SomeClass
			instanceVariableNames: ''''
			classVariableNames: ''Foo''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	model defineClass: 'SomeClass subclass: #Subclass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	model defineClass: 'Subclass subclass: #AnotherSubclass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	class := model metaclassNamed: #AnotherSubclass.
	class compile: 'bar ^Foo' classified: #(#accessing).
	class := class theNonMetaClass.
	
	(RBPushDownVariableTransformation
	 	model: model
	 	classVariable: #Foo
	 	class: class superclass superclass)
	 	asRefactoring transform.
			
	self deny: (class superclass superclass directlyDefinesClassVariable: #Foo).
	self deny: (class superclass directlyDefinesClassVariable: #Foo).
	self assert: (class directlyDefinesClassVariable: #Foo)
]

{ #category : #testing }
RBPushDownVariableTransformationTest >> testClassVariableToMultipleSubclassesFailure [

	model defineClass: 'Object subclass: #SomeClass
			instanceVariableNames: ''''
			classVariableNames: ''Foo''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	model defineClass: 'SomeClass subclass: #Subclass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	(model classNamed: #Subclass) compile: 'foo ^Foo' classified: #(#accessing).
	model defineClass: 'SomeClass subclass: #AnotherSubclass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	(model metaclassNamed: #AnotherSubclass) compile: 'bar ^Foo' classified: #(#accessing).
		
	self shouldFail: (RBPushDownVariableTransformation
							model: model
							classVariable: #Foo
							class: (model classNamed: #SomeClass))
							asRefactoring 
]

{ #category : #testing }
RBPushDownVariableTransformationTest >> testInstanceVariable [

	| refactoring |
	refactoring := (RBPushDownVariableTransformation
						instanceVariable: 'foo1'
						class: #RBDummyLintRuleTest)
						asRefactoring transform.
	
	(refactoring model classNamed: #RBDummyLintRuleTest)
	subclasses do: [ :each | self assert: (each directlyDefinesInstanceVariable: 'foo1') ]
]

{ #category : #testing }
RBPushDownVariableTransformationTest >> testInstanceVariableToMultipleSubclasses [

	model defineClass: 'Object subclass: #SomeClass
			instanceVariableNames: ''foo''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	model defineClass: 'SomeClass subclass: #Subclass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	(model classNamed: #Subclass) compile: 'foo ^foo' classified: #(#accessing).
	model defineClass: 'SomeClass subclass: #AnotherSubclass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	(model classNamed: #AnotherSubclass) compile: 'bar ^foo' classified: #(#accessing).
		
	(RBPushDownVariableTransformation 
	 	model: model
	 	instanceVariable: 'foo'
	 	class: (model classNamed: #SomeClass))
	 	asRefactoring transform.
				
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self assert: ((model classNamed: #Subclass) directlyDefinesVariable: 'foo').
	self assert: ((model classNamed: #AnotherSubclass) directlyDefinesVariable: 'foo')
]

{ #category : #testing }
RBPushDownVariableTransformationTest >> testMetaclass [

	model defineClass: 'Object subclass: #SomeClass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	(model metaclassNamed: #SomeClass) addInstanceVariable: 'foo'.
	model defineClass: 'SomeClass subclass: #Subclass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	(model metaclassNamed: #Subclass) compile: 'foo ^foo' classified: #(#accessing).
		
	(RBPushDownVariableTransformation
	 	model: model
	 	instanceVariable: 'foo'
	 	class: (model metaclassNamed: #SomeClass))
	 	asRefactoring transform.
				
	self deny: ((model metaclassNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self assert: ((model metaclassNamed: #Subclass) directlyDefinesVariable: 'foo')
]

{ #category : #testing }
RBPushDownVariableTransformationTest >> testModel [

	| class |
	model 	defineClass: 'Object subclass: #SomeClass
			instanceVariableNames: ''''
			classVariableNames: ''Foo'' 
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	model defineClass: 'SomeClass subclass: #Subclass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	class := model classNamed: #Subclass.
	class compile: 'foo ^Foo' classified: #(#accessing).
	
	(RBPushDownVariableTransformation 
		 model: model
		 classVariable: #Foo
		 class: class superclass)
		 asRefactoring transform.
				
	self deny: (class superclass directlyDefinesClassVariable: #Foo).
	self assert: (class directlyDefinesClassVariable: #Foo)
]

{ #category : #testing }
RBPushDownVariableTransformationTest >> testTransform [

	| transformation |
	transformation := (RBPushDownVariableTransformation 
							instanceVariable: 'foo1'
							class: #RBDummyLintRuleTest)
							transform.
		
	(transformation model classNamed: #RBDummyLintRuleTest) 
	subclasses do: [ :each | self assert: (each directlyDefinesInstanceVariable: 'foo1') ]
]

{ #category : #testing }
RBPushDownVariableTransformationTest >> testUnusedClassVariable [

	model defineClass: 'Object subclass: #SomeClass
			instanceVariableNames: ''''
			classVariableNames: ''Foo''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	self assert: ((model classNamed: #SomeClass) directlyDefinesVariable: #Foo).
	
	model defineClass: 'SomeClass subclass: #Subclass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
			
	(RBPushDownVariableTransformation
	 	model: model
	 	classVariable: #Foo
	 	class: (model classNamed: #SomeClass))
	 	asRefactoring transform.
	
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: #Foo).
	self deny: ((model classNamed: #Subclass) directlyDefinesVariable: #Foo)
]

{ #category : #testing }
RBPushDownVariableTransformationTest >> testUnusedInstanceVariable [

	model defineClass: 'Object subclass: #SomeClass
			instanceVariableNames: ''foo''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	model defineClass: 'SomeClass subclass: #Subclass 
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
		
	(RBPushDownVariableTransformation
		 model: model
		 instanceVariable: 'foo'
		 class: (model classNamed: #SomeClass))
		asRefactoring transform.
	
	self deny: ((model classNamed: #SomeClass) directlyDefinesVariable: 'foo').
	self assert: ((model classNamed: #Subclass) directlyDefinesVariable: 'foo')
]

{ #category : #testing }
RBPushDownVariableTransformationTest >> testVariableDoesNotExist [

	model defineClass: 'Object subclass: #SomeClass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: #''Refactory-Test data'''.
			
	self
		shouldFail: (RBPushDownVariableTransformation
						instanceVariable: 'foo'
						class: #RBBasicLintRuleTestData)
						asRefactoring;
		shouldFail: (RBPushDownVariableTransformation
						model: model
						classVariable: #Foo
						class: (model classNamed: #SomeClass))
						asRefactoring;
		shouldFail: (RBPushDownVariableTransformation 
						classVariable: #Foo
						class: #RBBasicLintRuleTestData)
						asRefactoring
]

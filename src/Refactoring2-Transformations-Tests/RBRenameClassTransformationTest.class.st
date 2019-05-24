Class {
	#name : #RBRenameClassTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBRenameClassTransformationTest >> testBadName [

	self
		shouldFail: (RBRenameClassTransformation
						rename: #RBDummyLintRuleTest
						to: self objectClassVariable)
						asRefactoring;
		shouldFail: (RBRenameClassTransformation
						rename: #RBDummyLintRuleTest
						to: #'Ob ject')
						asRefactoring
]

{ #category : #testing }
RBRenameClassTransformationTest >> testExistingName [

	self shouldFail: (RBRenameClassTransformation
							rename: self class
							to: #Object)
							asRefactoring
]

{ #category : #testing }
RBRenameClassTransformationTest >> testMetaClassFailure [

	self shouldFail: (RBRenameClassTransformation
							rename: self class class
							to: #Foo)
							asRefactoring
]

{ #category : #testing }
RBRenameClassTransformationTest >> testModelRenameSequenceClass [

	model defineClass: 'Object subclass: #Foo1 
			instanceVariableNames: '''' 
			classVariableNames: '''' poolDictionaries: ''''
			category: #''Refactory-Test data'''.
	(model classNamed: #Foo1)
		compile: 'foo	 ^ Foo1' classified: #(accessing);
		compile: 'objectName ^ #(Foo1)' classified: #(accessing).
		
	(RBRenameClassTransformation
		model: model
		rename: #Foo1
		to: #Foo2)
		asRefactoring transform.
	(RBRenameClassTransformation
		model: model
		rename: #Foo2
		to: #Foo3)
		asRefactoring transform.
		
	self deny: (model includesClassNamed: #Foo1).
	self deny: (model includesClassNamed: #Foo2).
	self assert: (model includesClassNamed: #Foo3).
	self assert: ((model classNamed: #Foo3) parseTreeFor: #foo) 
		  equals: (self parseMethod: 'foo ^ Foo3').
	self assert: ((model classNamed: #Foo3) parseTreeFor: #objectName) 
		  equals: (self parseMethod: 'objectName ^ #(Foo3)')
]

{ #category : #testing }
RBRenameClassTransformationTest >> testRefactoring [

	| refactoring class |
	model 	defineClass: 'Object subclass: #Foo
		instanceVariableNames: ''a''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: #''Refactoring2-Refactorings-Tests'''.
	class := model classNamed: #Foo.
	class compile: 'foo	^Object' classified: #(#accessing);
			compile: 'objectName ^#(Object)' classified: #(#accessing).
			
	refactoring := (RBRenameClassTransformation
						model: model
						rename: #Object
						to: #Thing)
						asRefactoring transform.
	
	self assert: (model includesClassNamed: #Thing).
	self deny: (model includesClassNamed: #Object).
	self assert: (class parseTreeFor: #foo) 
		  equals: (self parseMethod: 'foo ^Thing').
	self assert: (class parseTreeFor: #objectName) 
		  equals: (self parseMethod: 'objectName ^#(Thing)').
	self assert: class superclass name equals: #Thing
]

{ #category : #testing }
RBRenameClassTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBRenameClassTransformation
							rename: 'RBDummyClassToRename' asSymbol
							to: 'RBNewDummyClassName' asSymbol)
							transform.
							
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: 'RBNewDummyClassName' asSymbol.
	self assert: (class parseTreeFor: #method1)
		  equals: (self parseMethod: 'method1 ^ self method2').
	self deny: (transformation model includesClassNamed: 'RBDummyClassToRename' asSymbol).
				
	class := transformation model classNamed: 'RBDummySubclassOfClassToRename' asSymbol.
	self assert: class superclass 
		  equals: (transformation model classNamed: 'RBNewDummyClassName' asSymbol).
	self assert: (class parseTreeFor: #symbolReference) 
		  equals: (self parseMethod: 'symbolReference ^ #RBNewDummyClassName').
	self assert: (class parseTreeFor: #reference) 
		  equals: (self parseMethod: 'reference ^ RBNewDummyClassName new')
]

Class {
	#name : #RBTransformationsTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #initialization }
RBTransformationsTest >> setUp [

	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBTransformationsTest >> testAddClassTransform [
	| transformation newClass superclass |
	transformation := (RBAddClassTransformation
		addClass: self changeMock name , 'Temporary'
		superclass: #Object
		subclasses: OrderedCollection new
		category: self class category) transform.

	self assert: transformation model changes changes size equals: 1.

	newClass := transformation model classNamed: (self changeMock name , 'Temporary') asSymbol.
	superclass := transformation model classNamed: #Object.
	self assert: newClass superclass equals: superclass.
	self assert: (superclass subclasses includes: newClass).
	self assert: newClass theMetaClass superclass equals: superclass theMetaClass.
	self assert: (superclass theMetaClass subclasses includes: newClass theMetaClass)
]

{ #category : #tests }
RBTransformationsTest >> testAddMethodTransform [

	| transformation class |
	transformation := (RBAddMethodTransformation
		sourceCode: 'printString1 ^super printString'
		in: self changeMock name
		withProtocols: {#accessing})
		transform.
	
	class := transformation model classNamed: self changeMock name.
	self assert: (class parseTreeFor: #printString1)
		  equals: (self parseMethod: 'printString1 ^super printString')
]

{ #category : #tests }
RBTransformationsTest >> testAddVariableTransform [

	| transformation class |
	transformation := (RBAddVariableTransformation 
							instanceVariable: 'asdf'
							class: self changeMock name)
							transform. 
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self assert: (class directlyDefinesInstanceVariable: 'asdf').
]

{ #category : #tests }
RBTransformationsTest >> testPullUpVariableTransform [

	| transformation |
	transformation := (RBPullUpVariableTransformation
							instanceVariable: 'result'
							class: #RBDummyLintRuleTest)
							transform.
	
	self assert: ((transformation model classNamed: #RBDummyLintRuleTest)
			directlyDefinesInstanceVariable: 'result').
	
	self deny: ((transformation model classNamed: #RBBasicDummyLintRuleTest)
			directlyDefinesInstanceVariable: 'result').
	self deny: ((transformation model classNamed: #RBFooDummyLintRuleTest)
			directlyDefinesInstanceVariable: 'result')
]

{ #category : #tests }
RBTransformationsTest >> testPushDownVariableTransform [

	| transformation |
	transformation := (RBPushDownVariableTransformation 
							instanceVariable: 'foo1'
							class: #RBDummyLintRuleTest)
							transform.
		
	(transformation model classNamed: #RBDummyLintRuleTest) 
	subclasses do: [ :each | self assert: (each directlyDefinesInstanceVariable: 'foo1') ]
]

{ #category : #tests }
RBTransformationsTest >> testRenameClassTransform [

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

{ #category : #tests }
RBTransformationsTest >> testRenameTemporaryTransform [

	| transformation class |
	transformation := (RBAddMethodTransformation
							sourceCode: 'foo
									| temp bar |
									bar := 5.
									temp := bar * bar.
									Transcript show: temp printString; cr.
									^temp * temp'
							in: self changeMock name
							withProtocols: {#accessing})
							transform.
	
	transformation := (RBRenameTemporaryVariableTransformation 
							model: transformation model
							rename: #temp to: #temp2
							in: self changeMock name
							selector: #foo)
							transform.
	
	self assert: transformation model changes changes size equals: 2.			
	
	class := transformation model classNamed: self changeMock name.
	self assert: (class directlyDefinesMethod: #foo).		
	self assert: (class parseTreeFor: #foo) temporaries size equals: 2.
	self assert: ((class parseTreeFor: #foo) temporaries anySatisfy: [ :e | e name = #temp2 ])
]

{ #category : #tests }
RBTransformationsTest >> testRenameVariableTransform [

	| transformation class |
	transformation := (RBRenameVariableTransformation
							rename: 'classBlock' to: 'asdf'
							in: #RBBasicLintRuleTestData
							classVariable: false)
							transform.
	
	class := transformation model classNamed: #RBBasicLintRuleTestData.
	self assert: (class directlyDefinesInstanceVariable: 'asdf').
	self deny: (class directlyDefinesInstanceVariable: 'classBlock').
	self assert: (class parseTreeFor: #checkClass:)
		  equals: (self parseMethod:
				'checkClass: aSmalllintContext 
					^asdf value: aSmalllintContext value: result').
	self assert: (class parseTreeFor: #initialize)
		  equals: (self parseMethod:
				'initialize
					super initialize.
					self anInstVar: 1.
					asdf := [:context :aResult | ].
					methodBlock := [:context :aResult | ].
					self resultClass: RBSelectorEnvironment')
]

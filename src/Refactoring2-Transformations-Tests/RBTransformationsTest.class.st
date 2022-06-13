Class {
	#name : #RBTransformationsTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #running }
RBTransformationsTest >> setUp [

	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBTransformationsTest >> testAddClassTransform [

	| transformation class |
	transformation := (RBAddClassCommentTransformation
		comment: 'New comment for class'
		in: self changeMock name)
		transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name.
	self assert: class comment equals: 'New comment for class'
]

{ #category : #tests }
RBTransformationsTest >> testAddMethodCommentTransform [

	| transformation method |
	transformation := (RBAddMethodCommentTransformation
		comment: 'New comment for method'
		inMethod: #one
		inClass: self changeMock name)
		transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	method := (transformation model classNamed: self changeMock name) methodFor: #one.
	self assert: method ast comments first contents equals: 'New comment for method'
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
RBTransformationsTest >> testAddProtocolTransform [
	
	| transformation |
	transformation := (RBAddProtocolTransformation new
		protocol: 'transforming'
		inClass: #RBDummyEmptyClass)
		transform.
		
	self assert: transformation model changes changes size equals: 1
]

{ #category : #tests }
RBTransformationsTest >> testAddTemporaryVariableTransform [

	| transformation class |
	transformation := (RBAddTemporaryVariableTransformation
							variable: 'variable'
							inMethod: #one
							inClass: self changeMock name)
							transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self assert: (class directlyDefinesMethod: #one).
	self assert: (class parseTreeFor: #one) temporaries size equals: 1
]

{ #category : #tests }
RBTransformationsTest >> testAddVariableAccessorTransform [
	| transformation class |
	transformation := (RBAddVariableAccessorTransformation 
		instanceVariable: 'instVar' 
		class: self changeMock name) 
		transform.

	self assert: transformation model changes changes size equals: 2.

	class := transformation model classNamed: self changeMock name asSymbol.
	self assert: (class parseTreeFor: #instVar) 
		equals: (self parseMethod: 'instVar ^instVar').
	self assert: (class parseTreeFor: #instVar:) 
		equals: (self parseMethod: 'instVar: anObject instVar := anObject')
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
RBTransformationsTest >> testCompositeTransform [

	| transformation newClassName class |
	newClassName := (self changeMock name, 'Temporary') asSymbol.
	transformation := RBCompositeTransformation new
		transformations: (OrderedCollection new
			add: (RBAddClassTransformation
					addClass: newClassName
					superclass: #Object
					subclasses: #() 
					category: self class category);
			add: (RBAddVariableTransformation 
					instanceVariable: 'asdf'
					class: newClassName);
			add: (RBAddMethodTransformation
					sourceCode: 'printString1 ^super printString'
					in: newClassName
					withProtocols: {#accessing});
			yourself).
	transformation transform.
	
	self assert: transformation model changes changes size equals: 3.
	
	class := transformation model classNamed: (self changeMock name, 'Temporary') asSymbol.
	self assert: (class directlyDefinesInstanceVariable: 'asdf').
	self assert: (class parseTreeFor: #printString1)
		  equals: (self parseMethod: 'printString1 ^super printString')
]

{ #category : #tests }
RBTransformationsTest >> testCustomTransform [

	| transformation newClassName class |
	newClassName := (self changeMock name, 'Temporary') asSymbol.
	transformation := RBCustomTransformation with: [ :aModel |
		"add class"
		aModel
			defineClass: ('<1p> subclass: #<2s>
				instanceVariableNames: ''''
				classVariableNames: ''''
				poolDictionaries: ''''
				category: <3p>' 
			expandMacrosWith: 'Object'
			with: newClassName
			with: self class category).
		
		"add a comment in the class description"
		(aModel classNamed: newClassName) comment: 'Deprecated!!! Use super class'.
		"add an instance variable"
		(aModel classNamed: newClassName) addInstanceVariable: 'asdf' ].

	transformation transform.
	self assert: transformation model changes changes size equals: 3.
	
	class := transformation model classNamed: (self changeMock name, 'Temporary') asSymbol.
	self assert: (class directlyDefinesInstanceVariable: 'asdf')
]

{ #category : #tests }
RBTransformationsTest >> testDeprecateClassTransform [

	| transformation class |
	transformation := (RBDeprecateClassTransformation 
							class: self changeMock name)
							transform. 

	self assert: transformation model changes changes size equals: 4.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self assert: class comment equals: 'Deprecated!!! Use superclass'.
	
	class := transformation model metaclassNamed: self changeMock name.
	
	self assert: (class parseTreeFor: #new)
		equals: (self parseMethod: 'new
				self deprecated: ''Use superclass '' on: ''4 May 2016''  in: #Pharo60.
				^ super new').
	self assert: (class parseTreeFor: #deprecated)
		equals: (self parseMethod: 'deprecated ^ true').
	self assert: (class parseTreeFor: #systemIcon)
		equals: (self parseMethod: 'systemIcon
				^ Smalltalk ui icons iconNamed: #packageDelete').
]

{ #category : #tests }
RBTransformationsTest >> testMoveInstanceVariableToClassTransform [

	| transformation oldClass newClass |
	model := RBNamespace new.
	model defineClass: 'Object subclass: #FOOBAR 
				instanceVariableNames: ''asdf'' 	
				classVariableNames: ''''
				poolDictionaries: ''''
				category: #''Refactory-Test data'''.
				
	transformation := RBMoveInstanceVariableToClassTransformation
							model: model
							variable: 'asdf'
							fromClass: #FOOBAR
							toClass: self changeMock name.
	
	oldClass := transformation model classNamed: #FOOBAR.
	newClass := transformation model classNamed: self changeMock name asSymbol.
	self assert: (oldClass directlyDefinesInstanceVariable: 'asdf').
	self deny: (newClass directlyDefinesInstanceVariable: 'asdf').
	
	transformation transform.
	self assert: transformation model changes changes size equals: 3.
	
	oldClass := transformation model classNamed: #FOOBAR.
	newClass := transformation model classNamed: self changeMock name asSymbol.
	self deny: (oldClass directlyDefinesInstanceVariable: 'asdf').
	self assert: (newClass directlyDefinesInstanceVariable: 'asdf').
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
RBTransformationsTest >> testRemoveClassTransform [

	| transformation newClass superclass |
	transformation := (RBRemoveClassTransformation
							className: self changeMock name)
							transform.
	self assert: transformation model changes changes size equals: 1.
	newClass := transformation model classNamed: self changeMock name asSymbol.
	superclass := transformation model classNamed: #Object.
	self assert: newClass isNil.
	newClass := self changeMock name.
	self assert: ((superclass subclasses collect: [:each | each name]) includes: newClass) not.
	self assert: ((superclass classSide subclasses collect: [:each | each name]) includes: newClass) not.
]

{ #category : #tests }
RBTransformationsTest >> testRemoveMessageSendTransform [

	| transformation class methodName |
	methodName := ('called:', 'on:') asSymbol.
	transformation := (RBRemoveMessageSendTransformation new
		messageSend: #cr
		inMethod: methodName
		inClass: #RBRefactoryTestDataApp)
		transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: #RBRefactoryTestDataApp.
	self assert: (class parseTreeFor: methodName)
			equals:  (self parseMethod: 'called: anObject on: aBlock 
	Transcript show: anObject printString.
	aBlock value')
]

{ #category : #tests }
RBTransformationsTest >> testRemoveMethodTransform [

	| transformation class |
	transformation := (RBRemoveMethodTransformation 
		selector: #one
		from: self changeMock name)
		transform. 
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self deny: (class directlyDefinesMethod: #one).
]

{ #category : #tests }
RBTransformationsTest >> testRemoveVariableTransform [

	| transformation class |
	transformation := (RBRemoveVariableTransformation 
							instanceVariable: 'instVar'
							class: self changeMock name)
							transform. 
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self changeMock name asSymbol.
	self deny: (class directlyDefinesInstanceVariable: 'instVar').
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

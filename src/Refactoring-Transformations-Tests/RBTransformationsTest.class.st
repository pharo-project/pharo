Class {
	#name : 'RBTransformationsTest',
	#superclass : 'RBAbstractTransformationTest',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'running' }
RBTransformationsTest >> setUp [

	super setUp.
	model := self rbModelForVariableTest
]

{ #category : 'tests' }
RBTransformationsTest >> testAddClassTransform [

	| transformation class |
	transformation := RBAddClassCommentTransformation
		                  comment: 'New comment for class'
		                  in: self changeMockClass name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self changeMockClass name.
	self assert: class comment equals: 'New comment for class'
]

{ #category : 'tests' }
RBTransformationsTest >> testAddMethodCommentTransform [

	| transformation method |
	transformation := RBAddMethodCommentTransformation
		                  comment: 'New comment for method'
		                  inMethod: #one
		                  inClass: self changeMockClass name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	method := (transformation model classNamed: self changeMockClass name)
		          methodFor: #one.
	self
		assert: method ast comments first contents
		equals: 'New comment for method'
]

{ #category : 'tests' }
RBTransformationsTest >> testAddMethodCommentTransformWithExistingQuotationMarks [

	| transformation method |
	transformation := RBAddMethodCommentTransformation
		                  comment: '"New comment for method"'
		                  inMethod: #one
		                  inClass: self changeMockClass name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	method := (transformation model classNamed: self changeMockClass name)
		          methodFor: #one.
	self
		assert: method ast comments first contents
		equals: 'New comment for method'
]

{ #category : 'tests' }
RBTransformationsTest >> testAddMethodTransform [

	| transformation class |
	transformation := RBAddMethodTransformation
		                  sourceCode: 'printString1 ^super printString'
		                  in: self changeMockClass name
		                  withProtocol: #accessing.
	transformation generateChanges.

	class := transformation model classNamed: self changeMockClass name.
	self
		assert: (class parseTreeForSelector: #printString1)
		equals: (self parseMethod: 'printString1 ^super printString')
]

{ #category : 'tests' }
RBTransformationsTest >> testAddProtocolTransform [

	[
	| transformation |
	transformation := RBAddProtocolTransformation new
		                  protocol: #transforming
		                  inClass: #RBDummyEmptyClass.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1 ]
		ensure: [ RBDummyEmptyClass removeProtocolIfEmpty: #transforming ]
]

{ #category : 'tests' }
RBTransformationsTest >> testAddTemporaryVariableTransform [

	| transformation class |
	transformation := RBAddTemporaryVariableTransformation
		                  variable: 'variable'
		                  inMethod: #one
		                  inClass: self changeMockClass name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.
	class := transformation model classNamed: self changeMockClass name.
	self assert: (class directlyDefinesMethod: #one).
	self
		assert: (class parseTreeForSelector: #one) temporaries size
		equals: 1
]

{ #category : 'tests' }
RBTransformationsTest >> testAddVariableAccessorTransform [

	| transformation class |
	transformation := RBAddVariableAccessorTransformation
		                  instanceVariable: 'instVar'
		                  class: self changeMockClass name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 2.

	class := transformation model classNamed: self changeMockClass name.
	self
		assert: (class parseTreeForSelector: #instVar)
		equals: (self parseMethod: 'instVar ^instVar').
	self
		assert: (class parseTreeForSelector: #instVar:)
		equals: (self parseMethod: 'instVar: anObject instVar := anObject')
]

{ #category : 'tests' }
RBTransformationsTest >> testAddVariableTransform [

	| transformation class |
	transformation := RBAddVariableTransformation
		                  instanceVariable: 'asdf'
		                  class: self changeMockClass name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.
	class := transformation model classNamed: self changeMockClass name.
	self assert: (class directlyDefinesInstanceVariable: 'asdf')
]

{ #category : 'tests' }
RBTransformationsTest >> testCompositeTransform [

	| transformation newClassName class |
	newClassName := (self changeMockClass name , 'Temporary') asSymbol.
	transformation := RBCompositeTransformation new transformations: (OrderedCollection new
			                   add: ((RBInsertNewClassTransformation className: newClassName)
					                    superclass: #Object;
					                    packageName: self class package name);
			                   add: (RBAddVariableTransformation instanceVariable: 'asdf' class: newClassName);
			                   add: (RBAddMethodTransformation sourceCode: 'printString1 ^super printString' in: newClassName withProtocol: #accessing);
			                   yourself).
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 3.

	class := transformation model classNamed: self changeMockClass name , 'Temporary'.
	self assert: (class directlyDefinesInstanceVariable: 'asdf').
	self assert: (class parseTreeForSelector: #printString1) equals: (self parseMethod: 'printString1 ^super printString')
]

{ #category : 'tests' }
RBTransformationsTest >> testCustomTransform [

	| transformation newClassName class |
	newClassName := (self changeMockClass name , 'Temporary') asSymbol.
	transformation := RBCustomTransformation with: [ :aModel | "add class"
		                  aModel defineClass: [ :aBuilder |
			                  aBuilder
				                  superclass: Object;
				                  name: newClassName;
				                  package: self class package name ].
		                  "add a comment in the class description"
		                  (aModel classNamed: newClassName) comment: 'Deprecated!!! Use super class'.
		                  "add an instance variable"
		                  (aModel classNamed: newClassName) addInstanceVariable: 'asdf' ].

	transformation generateChanges.
	self assert: transformation model changes changes size equals: 3.

	class := transformation model classNamed: (self changeMockClass name , 'Temporary') asSymbol.
	self assert: (class directlyDefinesInstanceVariable: 'asdf')
]

{ #category : 'tests' }
RBTransformationsTest >> testDeprecateClassTransform [

	| transformation class |
	transformation := RBDeprecateClassTransformation className:
		                  self changeMockClass name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model metaclassNamed:
		         self changeMockClass name.
	self
		assert: (class parseTreeForSelector: #isDeprecated)
		equals: (self parseMethod: 'isDeprecated ^ true')
]

{ #category : 'tests' }
RBTransformationsTest >> testMoveInstanceVariableToClassTransform [

	| transformation oldClass newClass |
	model := RBNamespace new.
	model defineClass: [ :aBuilder |
		aBuilder
			superclass: Object;
			name: #FOOBAR;
			slots: { #asdf };
			package: #'Refactory-Test data' ].

	transformation := RBMoveInstanceVariableToClassTransformation
		                  model: model
		                  variable: 'asdf'
		                  fromClass: #FOOBAR
		                  toClass: self changeMockClass name.

	oldClass := transformation model classNamed: #FOOBAR.
	newClass := transformation model classNamed: self changeMockClass name.
	self assert: (oldClass directlyDefinesInstanceVariable: 'asdf').
	self deny: (newClass directlyDefinesInstanceVariable: 'asdf').

	transformation generateChanges.
	self assert: transformation model changes changes size equals: 3.

	oldClass := transformation model classNamed: #FOOBAR.
	newClass := transformation model classNamed: self changeMockClass name asSymbol.
	self deny: (oldClass directlyDefinesInstanceVariable: 'asdf').
	self assert: (newClass directlyDefinesInstanceVariable: 'asdf')
]

{ #category : 'tests' }
RBTransformationsTest >> testPullUpVariableTransform [

	| transformation |
	transformation := RBPullUpVariableTransformation
		                  instanceVariable: 'result'
		                  class: #RBDummyLintRuleTest.
	transformation generateChanges.

	self assert:
		((transformation model classNamed: #RBDummyLintRuleTest)
			 directlyDefinesInstanceVariable: 'result').

	self deny:
		((transformation model classNamed: #RBBasicDummyLintRuleTest)
			 directlyDefinesInstanceVariable: 'result').
	self deny:
		((transformation model classNamed: #RBFooDummyLintRuleTest)
			 directlyDefinesInstanceVariable: 'result')
]

{ #category : 'tests' }
RBTransformationsTest >> testPushDownVariableTransform [

	| transformation |
	transformation := RBPushDownVariableTransformation
		                  instanceVariable: 'foo1'
		                  class: #RBDummyLintRuleTest.
	transformation generateChanges.

	(transformation model classNamed: #RBDummyLintRuleTest) subclasses
		do: [ :each |
		self assert: (each directlyDefinesInstanceVariable: 'foo1') ]
]

{ #category : 'tests' }
RBTransformationsTest >> testRemoveClassTransform [

	| transformation newClass superclass |
	transformation := RBRemoveClassTransformation className:
		                  self changeMockClass name.
	transformation generateChanges.
	self assert: transformation model changes changes size equals: 1.
	newClass := transformation model classNamed:
		            self changeMockClass name.
	superclass := transformation model classNamed: #Object.
	self assert: newClass isNil.
	newClass := self changeMockClass name.
	self assert:
		((superclass subclasses collect: [ :each | each name ]) includes:
			 newClass) not.
	self assert:
		((superclass classSide subclasses collect: [ :each | each name ])
			 includes: newClass) not
]

{ #category : 'tests' }
RBTransformationsTest >> testRemoveMessageSendTransform [

	| transformation class |
	transformation := RBRemoveAllMessageSendsTransformation new
		                  messageSend: #cr
		                  inMethod: (#called: , #on:) asSymbol
		                  inClass: #RBClassDataForRefactoringTest.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed:
		         #RBClassDataForRefactoringTest.
	self
		assert: (class parseTreeForSelector: (#called: , #on:) asSymbol)
		equals: (self parseMethod: 'called: anObject on: aBlock
	anObject printString traceCr.
	aBlock value')
]

{ #category : 'tests' }
RBTransformationsTest >> testRemoveMethodTransform [

	| transformation class |
	transformation := RBRemoveMethodTransformation
		                  selector: #one
		                  from: self changeMockClass name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self changeMockClass name.
	self deny: (class directlyDefinesMethod: #one)
]

{ #category : 'tests' }
RBTransformationsTest >> testRemoveVariableTransform [

	| transformation class |
	transformation := RBRemoveVariableTransformation
		                  instanceVariable: 'instVar'
		                  class: self changeMockClass name.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: self changeMockClass name.
	self deny: (class directlyDefinesInstanceVariable: 'instVar')
]

{ #category : 'tests' }
RBTransformationsTest >> testRenameClassTransform [

	| transformation class newClassName oldClassName |
	newClassName := #RBNewDummyClassName.
	oldClassName := RBClassToRename name asSymbol.
	transformation := ReRenameClassRefactoring
		                  rename: oldClassName
		                  to: newClassName.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 1.

	class := transformation model classNamed: newClassName.
	self
		assert: (class parseTreeForSelector: #method1)
		equals: (self parseMethod: 'method1 ^ self method2').

	self deny: (transformation model includesClassNamed: oldClassName).

	class := transformation model classNamed:
		         RBSubclassOfClassToRename name asSymbol.

	self
		assert: class superclass
		equals:
		(transformation model classNamed: 'RBNewDummyClassName' asSymbol).

	self
		assert: (class parseTreeForSelector: #symbolReference)
		equals: (self parseMethod: 'symbolReference ^ #RBNewDummyClassName').
	self
		assert: (class parseTreeForSelector: #reference)
		equals: (self parseMethod: 'reference ^ RBNewDummyClassName new')
]

{ #category : 'tests' }
RBTransformationsTest >> testRenameTemporaryTransform [

	| transformation class |
	transformation := RBAddMethodTransformation
		                  sourceCode: 'foo
									| temp bar |
									bar := 5.
									temp := bar * bar.
									Transcript show: temp printString; cr.
									^temp * temp'
		                  in: self changeMockClass name
		                  withProtocol: #accessing.
	transformation generateChanges.

	transformation := RBRenameTemporaryVariableTransformation
		                  model: transformation model
		                  rename: #temp
		                  to: #temp2
		                  in: self changeMockClass name
		                  selector: #foo.
	transformation generateChanges.

	self assert: transformation model changes changes size equals: 2.

	class := transformation model classNamed: self changeMockClass name.
	self assert: (class directlyDefinesMethod: #foo).
	self
		assert: (class parseTreeForSelector: #foo) temporaries size
		equals: 2.
	self assert:
		((class parseTreeForSelector: #foo) temporaries anySatisfy: [ :e |
			 e name = #temp2 ])
]

{ #category : 'tests' }
RBTransformationsTest >> testRenameVariableTransform [

	| transformation class |
	transformation := RBRenameVariableTransformation
		                  rename: 'classBlock'
		                  to: 'asdf'
		                  in: #RBBasicLintRuleTestData
		                  classVariable: false.
	transformation generateChanges.

	class := transformation model classNamed: #RBBasicLintRuleTestData.
	self assert: (class directlyDefinesInstanceVariable: 'asdf').
	self deny: (class directlyDefinesInstanceVariable: 'classBlock').
	self
		assert: (class parseTreeForSelector: #checkClass:)
		equals: (self parseMethod: 'checkClass: aSmalllintContext
					^asdf value: aSmalllintContext value: result').
	self
		assert: (class parseTreeForSelector: #initialize)
		equals: (self parseMethod: 'initialize
					super initialize.
					self anInstVar: 1.
					asdf := [:context :aResult | ].
					methodBlock := [:context :aResult | ].
					self resultClass: RBSelectorEnvironment')
]

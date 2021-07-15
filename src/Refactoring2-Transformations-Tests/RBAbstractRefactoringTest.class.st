Class {
	#name : #RBAbstractRefactoringTest,
	#superclass : #RBAbstractEnvTestCase,
	#traits : 'TModelBuilder + TSetUpRefactorings',
	#classTraits : 'TModelBuilder classTrait + TSetUpRefactorings classTrait',
	#instVars : [
		'model',
		'rbClass'
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #accessing }
RBAbstractRefactoringTest >> constructor [
	
]

{ #category : #helpers }
RBAbstractRefactoringTest >> createRefactoringWithArguments: aParameterCollection [
	^ (rbClass perform: self constructor
			withArguments: aParameterCollection) asRefactoring
]

{ #category : #helpers }
RBAbstractRefactoringTest >> createRefactoringWithModel: aRBNamespace andArguments: aParameterCollection [
	^ (rbClass perform: #model: , self constructor
			withArguments: {aRBNamespace} , aParameterCollection) asRefactoring
]

{ #category : #parsing }
RBAbstractRefactoringTest >> executeRefactoring: aRefactoring [
	aRefactoring primitiveExecute.
]

{ #category : #parsing }
RBAbstractRefactoringTest >> executeRefactorings: aRefactoringColl [
	aRefactoringColl do: [ :ref | ref primitiveExecute]
]

{ #category : #mocking }
RBAbstractRefactoringTest >> extractMethodTestData [
	| newModel classEnvironment classes |
	classes := #(#MyClassA #MyClassB #MyClassC ) 
		inject: OrderedCollection new 
		into: [ :sum :each | 
			self environmentOfTest at: each ifPresent: [ :class | 
				sum add: class; add: class class ].
			sum ].
	classEnvironment := RBClassEnvironment classes: classes.
	newModel := self defaultNamespace onEnvironment: classEnvironment not.
	newModel name: 'Test'.
	
	#('Object subclass: #MyClassA
		instanceVariableNames: ''instVarName1 instVarName2''
		classVariableNames: ''ClassVarName1 ClassVarName2 ''
		poolDictionaries: ''''
		category: ''Testing'' '
	'MyClassA subclass: #MyClassB
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''Testing'' '
	'MyClassB subclass: #MyClassC
		instanceVariableNames: ''''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''Testing'' '
	) do: [ :each | 
		newModel defineClass: each ].
	
	#(#(#MyClassA #(
		#('bar
			^ self' #tests)
		#('name
			^ ''SomeString''' #tests)
		#('methodWithArg: anArg
			^ anArg not' #tests) 
		#('methodWithArg: anArg1 andArg: anArg2
			^ (anArg1 > anArg2) not' #tests) 
		#('problemCount
			^ 435' #tests)
		#('string: aString stream: aStream
			nil')
		#('myMethod
			| token |
			token := (String new: 100) writeStream.
			[ currentChar isLetter ] whileTrue: [ 
				token nextPut: currentChar.
				self methodWithArg: currentChar isLetter.
				self bar ].
			currentChar isLetter.
			$3 isLetter.
	 		currentChar isLetter.
			token :=  currentChar isLetter.
			token := ( currentChar isLetter) example.
			^ token contents' #tests)
		#('displayName
			| nameStream string|
			string := self name.
			nameStream := WriteStream on: (String new: 64).
		nameStream nextPutAll: string;
			nextPutAll: '' (''.
		self problemCount printOn: nameStream.
		nameStream nextPut: $).
		^nameStream contents' #tests)))
	#(#MyClassB #(
		#('someMethod
			^ instVarName2 := 3' #tests)
		#('exampleMethod
			^ (currentChar isLetter) not' #tests) 
		#('dummyMethod
			(3 > self someMethod) not' #tests)
		#('anotherMethod
			|aStream|
			aStream := WriteStream on: (String new: 128).
			aStream nextPutAll: ''dfgdf'';
				nextPutAll: '' (''.
			self problemCount printOn: aStream.
			aStream nextPut: $).' #tests)))
	#(#MyClassC #(
		#('methodWithArg: anArg
			(currentChar isLetter and: [anArg isDecimal])
			ifTrue: [^ self].
			^ nil' #tests)
		#('visitMonospace: aFormat
			aFormat children do: [ :each | each accept: self ].
			self fixBlockWithoutMicText: aFormat' #tests)
		#('visitStrike: aFormat
			aFormat children do: [ :each | each accept: self ].
			self fixBlockWithoutMicText: aFormat.
			#( 4 5 2 ) do: [ :val | val children do: [ :each | each accept: self ] ]' #tests)
		#('visitParagraph: aParagraph
			^ aParagraph children do: [:each | each accept: self ]' #tests)
		)))
		do: [:each | 
			| class |
			class := newModel classNamed: each first.
			each last do: [ :methodPair | 
				class 
					compile: methodPair first
					classified: methodPair last]].
	^ newModel
]

{ #category : #helpers }
RBAbstractRefactoringTest >> objectClassVariable [
	^Object classPool keys detect: [:each | true]
]

{ #category : #parsing }
RBAbstractRefactoringTest >> parseExpression: aString [
	^ self parserClass parseExpression: aString
]

{ #category : #parsing }
RBAbstractRefactoringTest >> parseMethod: aString [
	^ self parserClass parseMethod: aString
]

{ #category : #parsing }
RBAbstractRefactoringTest >> parserClass [
	^ RBParser
]

{ #category : #actions }
RBAbstractRefactoringTest >> proceedThroughWarning: aBlock [ 
	aBlock on: RBRefactoringWarning do: [ :ex | ex resume ]
]

{ #category : #accessing }
RBAbstractRefactoringTest >> rbClass: anObject [

	rbClass := anObject
]

{ #category : #running }
RBAbstractRefactoringTest >> setUp [
	super setUp.
	model := self defaultNamespace new
]

{ #category : #actions }
RBAbstractRefactoringTest >> shouldFail: aRefactoring [ 

	self proceedThroughWarning: [
		self should: [ aRefactoring primitiveExecute ] 
		raise: RBRefactoringError ]
]

{ #category : #actions }
RBAbstractRefactoringTest >> shouldWarn: aRefactoringColl [ 
	self 
		should: [ self executeRefactoring: aRefactoringColl ]
		raise: RBRefactoringWarning
]

{ #category : #actions }
RBAbstractRefactoringTest >> shouldntWarn: aRefactoring [ 
	self 
		shouldnt: [ self executeRefactoring: aRefactoring ]
		raise: RBRefactoringWarning
]

{ #category : #tests }
RBAbstractRefactoringTest >> testConditions [
	| condition newCondition |
	condition := RBCondition new 
				type: #false
				block: [false]
				errorString: 'false'.
	condition errorMacro: '<1?true:false>'.
	self deny: condition check.
	self assert: condition errorString equals: 'false'.
	self assert: condition not check.
	self assert: condition printString equals: 'false'.
	self assert: condition not printString equals: 'NOT false'.
	self deny: (condition not & condition) check.
	self 
		assert: (condition not & condition) printString equals: 'NOT false & false'.
	self assert: (condition & condition) not check.
	self assert: (condition | condition not) check.
	self deny: (newCondition := condition | condition) check.
	self assert: newCondition errorString equals: 'false AND false'.
	self assert: (condition not | condition not) check.
	self deny: (newCondition := condition & condition) check.
	self assert: newCondition errorString equals: 'false'.
	self assert: (condition not & condition not) check.
	self assert: (condition & condition) errorString equals: 'false OR false'
]

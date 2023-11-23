Class {
	#name : 'RBExtractMethodAndOccurrencesParametrizedTest',
	#superclass : 'RBAbstractRefactoringTest',
	#category : 'Refactoring-Transformations-Tests-SingleParametrized',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'SingleParametrized'
}

{ #category : 'tests' }
RBExtractMethodAndOccurrencesParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBExtractMethodAndOccurrences };
		yourself
]

{ #category : 'accessing' }
RBExtractMethodAndOccurrencesParametrizedTest >> constructor [
	^ #extract:from:in:
]

{ #category : 'running' }
RBExtractMethodAndOccurrencesParametrizedTest >> setUp [
	super setUp.
	model := self rbModelForExtractMethodTest
]

{ #category : 'tests' }
RBExtractMethodAndOccurrencesParametrizedTest >> testExtractMethodWithTwoArgsAndOcurrences [
	|class refactoring|
	class := model classNamed: #MyClassA.
	refactoring := self createRefactoringWithModel: model
		andArguments: { (116 to: 245) . #displayName . class }.
	self setupSearchInAllHierarchyFor: refactoring toReturn: true.
	self setupMethodNameFor: refactoring toReturn: #stringArg:streamArg:.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeForSelector: #stringArg:streamArg:) equals: (self parseMethod: 'stringArg: string streamArg: nameStream

	nameStream
		nextPutAll: string;
		nextPutAll: '' (''.
	self problemCount printOn: nameStream.
	nameStream nextPut: $)').
	
	self 
		assert: (class parseTreeForSelector: #displayName) 
		equals: (self parseMethod: 'displayName

	| nameStream string |
	string := self name.
	nameStream := WriteStream on: (String new: 64).
	self stringArg: string streamArg: nameStream.
	^ nameStream contents').
	class := model classNamed: #MyClassB.
	self assert: (class parseTreeForSelector: #anotherMethod) equals: (self parseMethod: 'anotherMethod

	| aStream |
	aStream := WriteStream on: (String new: 128).
	self stringArg: ''dfgdf'' streamArg: aStream')
]

{ #category : 'tests' }
RBExtractMethodAndOccurrencesParametrizedTest >> testExtractSimpleMethodAndOcurrences [
	|class refactoring|
	class := model classNamed: #MyClassA.
	refactoring := self createRefactoringWithModel: model
		andArguments: { (291 to: 311) . #myMethod . class }.
	self setupSearchInAllHierarchyFor: refactoring toReturn: true.
	self setupMethodNameFor: refactoring toReturn: #extractedMethod.
	self executeRefactoring: refactoring.

	self 
		assert: (class parseTreeForSelector: #extractedMethod) 
		equals: (self parseMethod: 'extractedMethod
	^ currentChar isLetter.').

	self 
		assert: (class parseTreeForSelector: #myMethod) 
		equals: (self parseMethod: (self methodSignatureStringForUnarySymbol: #myMethod),

'	| token |
	token := (String new: 100) writeStream.
	[ self extractedMethod ] whileTrue: [
		token nextPut: currentChar.
		self methodWithArg: self extractedMethod.
		self bar ].
	self extractedMethod.
	$3 isLetter.
	self extractedMethod.
	token := self extractedMethod.
	token := self extractedMethod example.
	^ token contents').
	
	
	class := model classNamed: #MyClassB.
	self assert: (class parseTreeForSelector: #exampleMethod) 
	equals: (self parseMethod: (self methodSignatureStringForUnarySymbol: #exampleMethod),

'	^ self extractedMethod not').
	
	
	class := model classNamed: #MyClassC.
	self 
		assert: (class parseTreeForSelector: #methodWithArg:) 
		equals: (self parseMethod: (self methodSignatureStringForSymbol: #methodWithArg: withArgumentString: 'anArg'),

'	(self extractedMethod and: [ anArg isDecimal ]) ifTrue: [ ^ self ].
	^ nil')
]

{ #category : 'failure tests' }
RBExtractMethodAndOccurrencesParametrizedTest >> testFailureBadInterval [
	|class|
	class := model classNamed: #MyClassA.
	self shouldFail: (self createRefactoringWithModel: model
		andArguments: { (127 to: 136) . #myMethod . class })
]

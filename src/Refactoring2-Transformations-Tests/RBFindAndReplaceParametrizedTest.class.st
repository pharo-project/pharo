Class {
	#name : #RBFindAndReplaceParametrizedTest,
	#superclass : #RBAbstractRefactoringTest,
	#category : #'Refactoring2-Transformations-Tests-SingleParametrized'
}

{ #category : #'building suites' }
RBFindAndReplaceParametrizedTest class >> testParameters [
	^ ParametrizedTestMatrix new
		addCase: { #rbClass -> RBFindAndReplaceTransformation };
		yourself
]

{ #category : #accessing }
RBFindAndReplaceParametrizedTest >> constructor [
	^ #find:of:inWholeHierarchy:
]

{ #category : #running }
RBFindAndReplaceParametrizedTest >> setUp [
	super setUp.
	model := self extractMethodTestData
]

{ #category : #'failure tests' }
RBFindAndReplaceParametrizedTest >> testBadSelector [
	|class|
	class := model classNamed: #MyClassA.
	self shouldFail: (self createRefactoringWithModel: model
		andArguments: { #unexitingMethod . class . true })
]

{ #category : #'failure tests' }
RBFindAndReplaceParametrizedTest >> testFindOcurrencesFailureInHierarchy [
	|class refactoring|
	class := model classNamed: #MyClassA.
	class
		compile: 'simpleMethod
			^ currentChar isLetter'
		classified: #(#accessing).
	(model classNamed: #MyClassB)
		compile: 'simpleMethod
			^ nil'
		classified: #(#accessing).
	refactoring := self createRefactoringWithModel: model
		andArguments: { #simpleMethod . class . true }.
	self shouldFail: refactoring
]

{ #category : #tests }
RBFindAndReplaceParametrizedTest >> testFindOcurrencesInClass [
	|class refactoring|
	class := model classNamed: #MyClassA.
	class
		compile: 'simpleMethod
			^ currentChar isLetter'
		classified: #(#accessing).
	refactoring := self createRefactoringWithModel: model
		andArguments: {#simpleMethod . class . false }.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeForSelector: #myMethod) equals: (self parseMethod: 'myMethod

	| token |
	token := (String new: 100) writeStream.
	[ self simpleMethod ] whileTrue: [
		token nextPut: currentChar.
		self methodWithArg: self simpleMethod.
		self bar ].
	self simpleMethod.
	$3 isLetter.
	self simpleMethod.
	token := self simpleMethod.
	token := self simpleMethod example.
	^ token contents').
	class := model classNamed: #MyClassB.
	self assert: (class parseTreeForSelector: #exampleMethod) equals: (self parseMethod: 'exampleMethod

	^ currentChar isLetter not').
	class := model classNamed: #MyClassC.
	self assert: (class parseTreeForSelector: #methodWithArg:) equals: (self parseMethod: 'methodWithArg: anArg

	(currentChar isLetter and: [ anArg isDecimal ]) ifTrue: [ ^ self ].
	^ nil')
]

{ #category : #tests }
RBFindAndReplaceParametrizedTest >> testFindOcurrencesInHierarchy [
	|class refactoring|
	class := model classNamed: #MyClassA.
	class
		compile: 'simpleMethod
			^ currentChar isLetter'
		classified: #(#accessing).
	refactoring := self createRefactoringWithModel: model
		andArguments: { #simpleMethod . class . true }.
	self executeRefactoring: refactoring.
	self assert: (class parseTreeForSelector: #myMethod) equals: (self parseMethod: 'myMethod

	| token |
	token := (String new: 100) writeStream.
	[ self simpleMethod ] whileTrue: [
		token nextPut: currentChar.
		self methodWithArg: self simpleMethod.
		self bar ].
	self simpleMethod.
	$3 isLetter.
	self simpleMethod.
	token := self simpleMethod.
	token := self simpleMethod example.
	^ token contents').
	class := model classNamed: #MyClassB.
	self assert: (class parseTreeForSelector: #exampleMethod) equals: (self parseMethod: 'exampleMethod

	^ self simpleMethod not').
	class := model classNamed: #MyClassC.
	self assert: (class parseTreeForSelector: #methodWithArg:) equals: (self parseMethod: 'methodWithArg: anArg

	(self simpleMethod and: [ anArg isDecimal ]) ifTrue: [ ^ self ].
	^ nil')
]

{ #category : #tests }
RBFindAndReplaceParametrizedTest >> testFindOcurrencesWithArgInHierarchy [
	|class refactoring|
	class := model classNamed: #MyClassA.
	class
		compile: 'simpleMethod
			^ currentChar isLetter'
		classified: #(#accessing).
	refactoring := self createRefactoringWithModel: model
		andArguments: { #methodWithArg:andArg: . class . true }.
	self executeRefactoring: refactoring.
	class := model classNamed: #MyClassB.
	self assert: (class parseTreeForSelector: #dummyMethod) equals: (self parseMethod:
	'dummyMethod self methodWithArg: 3 andArg: self someMethod')
]

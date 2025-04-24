Class {
	#name : #RBAbstractRefactoringTest,
	#superclass : #RBAbstractEnvTestCase,
	#traits : 'TModelBuilder + TSetUpRefactorings',
	#classTraits : 'TModelBuilder classTrait + TSetUpRefactorings classTrait',
	#instVars : [
		#model,
		#rbClass
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
	aRefactoring primitiveExecute
]

{ #category : #parsing }
RBAbstractRefactoringTest >> executeRefactorings: aRefactoringColl [
	aRefactoringColl do: [ :ref | ref primitiveExecute]
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
				block: [false]
				errorString: 'false'.
	condition errorMacro: '<1?true:false>'.
	self deny: condition check.
	self assert: condition errorString equals: 'false'.
	self assert: condition not check.
	self deny: (condition not & condition) check.
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

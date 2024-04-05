Class {
	#name : 'MethodConstantTest',
	#superclass : 'TestCase',
	#category : 'Reflectivity-Tools-Tests',
	#package : 'Reflectivity-Tools-Tests'
}

{ #category : 'tests' }
MethodConstantTest >> constFromBlock: aBlock [

	^aBlock asMethodConstant
]

{ #category : 'tests' }
MethodConstantTest >> constFromReceiverExpression [

	^DateAndTime now asMethodConstant
]

{ #category : 'tests' }
MethodConstantTest >> sumOfTwoConsts: firstBlock and: secondBlock [

	^firstBlock asMethodConstant + secondBlock asMethodConstant
]

{ #category : 'running' }
MethodConstantTest >> tearDown [
	"recompile method to remove the caching"
	(self class>>#constFromReceiverExpression) recompile.
	(self class>>#sumOfTwoConsts:and:) recompile.
	(self class>>#constFromBlock:) recompile.
	super tearDown
]

{ #category : 'tests' }
MethodConstantTest >> testAPIFromBlock [
	| constInitialResult constSecondResult |
	constInitialResult := self constFromBlock: [ DateAndTime now ].
	constSecondResult := self constFromBlock: nil.

	self assert: constSecondResult identicalTo: constInitialResult
]

{ #category : 'tests' }
MethodConstantTest >> testAPIFromMessageSendReceiver [
	| constInitialResult constSecondResult |
	constInitialResult := self constFromReceiverExpression.
	constSecondResult := self constFromReceiverExpression.

	self assert: constSecondResult identicalTo: constInitialResult
]

{ #category : 'tests' }
MethodConstantTest >> testConstPlaceInTransformedMethod [

	| transformedMethod const |
	const := self constFromReceiverExpression.

	transformedMethod := self class >> #constFromReceiverExpression.
	self assert: const identicalTo: self constFromReceiverExpression.

	self assert:
		(transformedMethod allLiterals anySatisfy: [ :each | each == const ])
]

{ #category : 'tests' }
MethodConstantTest >> testTwoConstsInSameMethod [

	| result1 result2 |
	result1 := self sumOfTwoConsts: [ 1 ] and: [ 2 ].
	result2 := self sumOfTwoConsts: nil and: nil.
	self assert: result1 equals: 3.
	self assert: result2 equals: 3
]

{ #category : 'tests' }
MethodConstantTest >> testUsingConstJustInSameMethod [
	| values |
	values := OrderedCollection new.
	2 timesRepeat: [ values add: self constFromReceiverExpression ].

	self assert: values first identicalTo: values last
]

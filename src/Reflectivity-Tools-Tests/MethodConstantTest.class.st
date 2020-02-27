Class {
	#name : #MethodConstantTest,
	#superclass : #TestCase,
	#category : #'Reflectivity-Tools-Tests'
}

{ #category : #tests }
MethodConstantTest >> constFromBlock: aBlock [

	^aBlock asMethodConstant 

]

{ #category : #tests }
MethodConstantTest >> constFromReceiverExpression [

	^DateAndTime now asMethodConstant 

]

{ #category : #tests }
MethodConstantTest >> sumOfTwoConsts: firstBlock and: secondBlock [

	^firstBlock asMethodConstant + secondBlock asMethodConstant
]

{ #category : #tests }
MethodConstantTest >> testAPIFromBlock [
	| constInitialResult constSecondResult |
	constInitialResult := self constFromBlock: [ DateAndTime now ].
	constSecondResult := self constFromBlock: nil.

	self assert: constSecondResult identicalTo: constInitialResult
]

{ #category : #tests }
MethodConstantTest >> testAPIFromMessageSendReceiver [
	| constInitialResult constSecondResult |
	constInitialResult := self constFromReceiverExpression.
	constSecondResult := self constFromReceiverExpression.

	self assert: constSecondResult identicalTo: constInitialResult
]

{ #category : #tests }
MethodConstantTest >> testConstPlaceInTransformedMethod [

	| transformedMethod const constLiteral |
	const := self constFromReceiverExpression. 
		
	transformedMethod := self class >> #constFromReceiverExpression. 
	
	constLiteral := transformedMethod literals detect: [ :each | each value == const ].
	self assert: (constLiteral key includesSubstring: 'meta' caseSensitive: false)
]

{ #category : #tests }
MethodConstantTest >> testTwoConstsInSameMethod [

	| result1 result2 |
	result1 := self sumOfTwoConsts: [ 1 ] and: [ 2 ].
	result2 := self sumOfTwoConsts: nil and: nil.
	self assert: result1 equals: 3.
	self assert: result2 equals: 3.
]

{ #category : #tests }
MethodConstantTest >> testUsingConstJustInSameMethod [
	| values |
	self skip.	"not working for the first call: would need on stack replacement"
	values := OrderedCollection new.
	2 timesRepeat: [ values add: DateAndTime now asMethodConstant ].

	self assert: values first identicalTo: values last
]

Class {
	#name : #MethodConstantTests,
	#superclass : #TestCase,
	#category : #Reflectivity-Tools-Tests
}

{ #category : #tests }
MethodConstantTests >> constFromBlock: aBlock [

	^aBlock asMethodConstant 

]

{ #category : #tests }
MethodConstantTests >> constFromReceiverExpression [

	^DateAndTime now asMethodConstant 

]

{ #category : #tests }
MethodConstantTests >> sumOfTwoConsts: firstBlock and: secondBlock [

	^firstBlock asMethodConstant + secondBlock asMethodConstant
]

{ #category : #tests }
MethodConstantTests >> testAPIFromBlock [

	| constInitialResult constSecondResult |
	constInitialResult := self constFromBlock: [DateAndTime now].
	constSecondResult := self constFromBlock: nil.
	
	self assert: constSecondResult == constInitialResult
]

{ #category : #tests }
MethodConstantTests >> testAPIFromMessageSendReceiver [

	| constInitialResult constSecondResult |
	constInitialResult := self constFromReceiverExpression.
	constSecondResult := self constFromReceiverExpression.
	
	self assert: constSecondResult == constInitialResult
]

{ #category : #tests }
MethodConstantTests >> testConstPlaceInTransformedMethod [

	| transformedMethod const constLiteral |
	const := self constFromReceiverExpression. 
		
	transformedMethod := self class >> #constFromReceiverExpression. 
	
	constLiteral := transformedMethod literals detect: [ :each | each value == const ].
	self assert: (constLiteral key includesSubstring: 'meta' caseSensitive: false)
]

{ #category : #tests }
MethodConstantTests >> testTwoConstsInSameMethod [

	| result1 result2 |
	result1 := self sumOfTwoConsts: [ 1 ] and: [ 2 ].
	result2 := self sumOfTwoConsts: nil and: nil.
	self assert: result1 equals: 3.
	self assert: result2 equals: 3.
]

{ #category : #tests }
MethodConstantTests >> testUsingConstJustInSameMethod [

	| values |
	<expectedFailure>
	values := OrderedCollection new.
	2 timesRepeat: [ values add: DateAndTime now asMethodConstant ].
	
	self assert: values first == values last
]

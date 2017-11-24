Class {
	#name : #GTSpotterExceptionsTest,
	#superclass : #TestCase,
	#instVars : [
		'spotter'
	],
	#classInstVars : [
		'exceptions',
		'fatals'
	],
	#category : #'GT-Tests-Spotter-Exceptions'
}

{ #category : #accessing }
GTSpotterExceptionsTest class >> exceptions [
	^ exceptions ifNil: [ exceptions := OrderedCollection new ]
]

{ #category : #accessing }
GTSpotterExceptionsTest class >> fatals [
	^ fatals ifNil: [ fatals := OrderedCollection new ]
]

{ #category : #accessing }
GTSpotterExceptionsTest class >> reset [
	exceptions := nil.
	fatals := nil.
]

{ #category : #private }
GTSpotterExceptionsTest >> assertException: anErrorClass [
	self deny: self class exceptions isEmpty.
	self assert: (self class exceptions anySatisfy: [ :e | e class == anErrorClass ]).
	self assert: self class fatals isEmpty.
]

{ #category : #private }
GTSpotterExceptionsTest >> assertNoExceptions [
	self assert: self class exceptions isEmpty.
	self assert: self class fatals isEmpty.
]

{ #category : #private }
GTSpotterExceptionsTest >> assertResetExceptions [
	self class reset.
	self assertNoExceptions.
]

{ #category : #private }
GTSpotterExceptionsTest >> exceptionHandler: anExceptionHandler do: aBlock [
	self assertResetExceptions.
	self shouldnt: [ 
		anExceptionHandler handleExceptionsDuring: aBlock ] raise: Error.
	self assertNoExceptions.
	
	self assertResetExceptions.
	self shouldnt: [ 
		spotter exceptionHandler: anExceptionHandler.
		spotter handleExceptionsDuring: aBlock ] raise: Error.
	self assertNoExceptions.
]

{ #category : #private }
GTSpotterExceptionsTest >> process: aString origin: anOrigin [
	| step |
	((step := GTSpotterStep new)
		spotter: spotter;
		origin: anOrigin;
		yourself) processContext: (step newContextForQuery: aString)
]

{ #category : #running }
GTSpotterExceptionsTest >> setUp [
	super setUp.
	
	spotter := GTSpotter new.
	self class reset.
]

{ #category : #private }
GTSpotterExceptionsTest >> shouldRaise: anError process: aString origin: anOrigin [
	self shouldRaise: anError process: aString origin: anOrigin do: [ :exception | ]
]

{ #category : #private }
GTSpotterExceptionsTest >> shouldRaise: anError process: aString origin: anOrigin do: aBlock [
	self shouldRaise: anError process: aString origin: anOrigin during: GTSpotterDebuggingExceptionHandlerMock new do: aBlock
]

{ #category : #private }
GTSpotterExceptionsTest >> shouldRaise: anErrorClass process: aString origin: anOrigin during: exceptionHandler [
	self shouldRaise: anErrorClass process: aString origin: anOrigin during: exceptionHandler do: [ :e | ]
]

{ #category : #private }
GTSpotterExceptionsTest >> shouldRaise: anErrorClass process: aString origin: anOrigin during: exceptionHandler do: aBlock [
	self assertResetExceptions.

	spotter exceptionHandler: exceptionHandler.
	self assert: spotter exceptionHandler == exceptionHandler.
	
	self should: [ self process: aString origin: anOrigin ] 
		raise: anErrorClass 
		withExceptionDo: [ :exception | 
			self assert: exception class == anErrorClass.
			aBlock value: exception. ].
		
	self assert: spotter exceptionHandler == exceptionHandler.
	
	self assertException: anErrorClass
]

{ #category : #private }
GTSpotterExceptionsTest >> shouldntRaise: anError process: aString origin: anOrigin [
	self shouldntRaise: anError process: aString origin: anOrigin during: GTSpotterDeploymentExceptionHandlerMock new
]

{ #category : #private }
GTSpotterExceptionsTest >> shouldntRaise: anError process: aString origin: anOrigin during: exceptionHandler [
	spotter exceptionHandler: exceptionHandler.
	self assert: spotter exceptionHandler == exceptionHandler.
	
	self shouldnt: [ self process: aString origin: anOrigin ] raise: anError.
	
	self assert: spotter exceptionHandler == exceptionHandler.
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDebuggingExceptionHandlerDuring [
	| exceptionHandler |
	
	self exceptionHandler: (exceptionHandler := GTSpotterDebuggingExceptionHandlerMock new) do: [ 
		self shouldnt: [ GTCurrentSpotterExceptionHandler value ] raise: Error.
		self assert: GTCurrentSpotterExceptionHandler value notNil.
		self assert: GTCurrentSpotterExceptionHandler value == exceptionHandler ].
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDebuggingExceptionHandlerMock1 [
	
	self shouldRaise: AssertionFailure process: 'abc' origin: GTSpotterExceptionMock1 new do: [:e | 
		self assert: e messageText = GTSpotterExceptionMock1 name ].
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDebuggingExceptionHandlerMock2 [

	self shouldRaise: MessageNotUnderstood process: 'abc' origin: GTSpotterExceptionMock2 new.
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDebuggingExceptionHandlerMock3 [

	self shouldRaise: ZeroDivide process: 'abc' origin: GTSpotterExceptionMock3 new.

]

{ #category : #tests }
GTSpotterExceptionsTest >> testDebuggingExceptionHandlerMock4 [

	self shouldRaise: ArithmeticError process: 'abc' origin: GTSpotterExceptionMock4 new do: [:e | 
		self assert: e messageText = 'Error while filtering' ].
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDebuggingExceptionHandlerMock5 [
	
	self assertResetExceptions.
	self shouldntRaise: AssertionFailure process: 'abc' origin: GTSpotterExceptionMock5 new during: GTSpotterDebuggingExceptionHandlerMock new.
	self assertNoExceptions.
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDeploymentExceptionHandlerDuring [
	| exceptionHandler |
	
	self exceptionHandler: (exceptionHandler := GTSpotterDeploymentExceptionHandlerMock new) do: [ 
		self shouldnt: [ GTCurrentSpotterExceptionHandler value ] raise: Error.
		self assert: GTCurrentSpotterExceptionHandler value notNil.
		self assert: GTCurrentSpotterExceptionHandler value == exceptionHandler ].
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDeploymentExceptionHandlerMock1 [
	
	self assertResetExceptions.
	self shouldntRaise: AssertionFailure process: 'abc' origin: GTSpotterExceptionMock1 new.
	self assertException: AssertionFailure.
	
	self assertResetExceptions.
	self shouldntRaise: Error process: 'abc' origin: GTSpotterExceptionMock1 new.
	self assertException: AssertionFailure.
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDeploymentExceptionHandlerMock2 [
	
	self assertResetExceptions.	
	self shouldntRaise: MessageNotUnderstood process: 'abc' origin: GTSpotterExceptionMock2 new.
	self assertException: MessageNotUnderstood.
	
	self assertResetExceptions.
	self shouldntRaise: Error process: 'abc' origin: GTSpotterExceptionMock2 new.
	self assertException: MessageNotUnderstood.
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDeploymentExceptionHandlerMock3 [
	
	self assertResetExceptions.
	self shouldntRaise: ZeroDivide process: 'abc' origin: GTSpotterExceptionMock3 new.
	self assertException: ZeroDivide.
	
	self assertResetExceptions.
	self shouldntRaise: Error process: 'abc' origin: GTSpotterExceptionMock3 new.
	self assertException: ZeroDivide.
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDeploymentExceptionHandlerMock4 [
	
	self assertResetExceptions.
	self shouldntRaise: ArithmeticError process: 'abc' origin: GTSpotterExceptionMock4 new.
	self assertException: ArithmeticError.
	
	self assertResetExceptions.
	self shouldntRaise: Error process: 'abc' origin: GTSpotterExceptionMock4 new.
	self assertException: ArithmeticError.
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDeploymentExceptionHandlerMock5 [
	
	self assertResetExceptions.
	self shouldntRaise: AssertionFailure process: 'abc' origin: GTSpotterExceptionMock5 new during: GTSpotterDeploymentExceptionHandlerMock new.
	self assertNoExceptions.
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDeploymentHandleException [

	self assertResetExceptions.
	self shouldnt: [ 
		[ 1 / 0 ] on: ZeroDivide do: [ :exception |
			self assert: (GTSpotterDeploymentExceptionHandlerMock new handleException: exception) notNil ] ] raise: ZeroDivide.
	self assertException: ZeroDivide.
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDeploymentHandleExceptionMock5 [
	
	self assertResetExceptions.
	self shouldnt: [ 
		[ 1 / 0 ] on: ZeroDivide do: [ :exception |
			self assert: (GTSpotterDeploymentExceptionHandlerMock5 new 
				handleException: exception 
				do: [ 7 ]) == 7 ] ] raise: ZeroDivide.
	self assertException: ZeroDivide
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDevelopmentHandleException [

	self assertResetExceptions.
	self should: [
		[ 1 / 0 ] on: ZeroDivide do: [ :exception |
			GTSpotterDebuggingExceptionHandlerMock new handleException: exception ] ] raise: ZeroDivide.
	self assertException: ZeroDivide.
]

{ #category : #tests }
GTSpotterExceptionsTest >> testDevelopmentHandleExceptionMock5 [

	self assertResetExceptions.
	self should: [ 
		[ 1 / 0 ] on: ZeroDivide do: [ :exception |
			GTSpotterDebuggingExceptionHandlerMock new 
				handleException: exception 
				do: [ self fail: 'Ups, we shouldnt have reached this statement' ] ] ] raise: ZeroDivide.
	self assertException: ZeroDivide
]

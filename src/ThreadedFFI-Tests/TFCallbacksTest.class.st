Class {
	#name : #TFCallbacksTest,
	#superclass : #TFTestCase,
	#instVars : [
		'oldExceptionHandler'
	],
	#category : #'ThreadedFFI-Tests'
}

{ #category : #utilities }
TFCallbacksTest >> callCallback: aCallback [
	
	| functionDefinition function |
	functionDefinition := TFFunctionDefinition
		parameterTypes: {}
		returnType: TFBasicType void.
	function := TFExternalFunction fromAddress: aCallback getHandle 
		definition: functionDefinition.

	runner invokeFunction: function withArguments: {}
]

{ #category : #'instance creation' }
TFCallbacksTest >> newTestCallbackDoing: aBlock [
	
	^ TFCallback 
		forCallback: aBlock 
		parameters: {} 
		returnType: TFBasicType void
		runner: runner.
]

{ #category : #running }
TFCallbacksTest >> setUp [

	super setUp.
	
	oldExceptionHandler := runner exceptionHandler.
	runner exceptionHandler: TFTestCallbackExceptionHandler new
]

{ #category : #running }
TFCallbacksTest >> tearDown [ 

	runner exceptionHandler: oldExceptionHandler.
	super tearDown
]

{ #category : #tests }
TFCallbacksTest >> testCallbackFromOldSessionFailsReturn [
	"If a callback survives a session (e.g., an image is saved and reopened), returning it will fail.
	This test validates a surviving callback throws a meaningful exception when trying to return in this case."
	
	| smain s1 c1 |
	smain := Semaphore new.

	s1 := Semaphore new.
	c1 := self newTestCallbackDoing: [ smain signal. s1 wait ].

	[ self callCallback: c1 ] fork.

	"wait until callback starts"
	smain wait.

	"Release the runner as if a new session started"
	runner release.

	"make callback finish and wait until it fails"
	s1 signal.
	runner exceptionHandler wait.

	self assert: runner exceptionHandler lastException class equals: TFInvalidSessionCallbackReturn.
	self assert: runner exceptionHandler lastException callback equals: c1.
]

{ #category : #tests }
TFCallbacksTest >> testCallbackNotRespectingLIFOOrderFailsReturn [

	| smain s1 c1 s2 c2 returned |
	smain := Semaphore new.

	true ifTrue: [ 	^ self skip ].

	returned := false.

	s1 := Semaphore new.
	c1 := self newTestCallbackDoing: [ smain signal. s1 wait ].

	s2 := Semaphore new.
	c2 := self newTestCallbackDoing: [ smain signal. s2 wait ].
	
	[ self callCallback: c1. returned := true ] fork.
	[ self callCallback: c2 ] fork.

	"wait until both callbacks arrive"
	smain wait; wait.
	
	"make callback 1 to finish while callback 2 is suspended"
	s1 signal.
	"Wait until C1 tries to return"
	100 milliSecond wait.
	"It should not return until C2 returns"
	self deny: returned.
	"Signal s2 to make c2 return"
	s2 signal.	
	"Wait until C1 tries to return"
	100 milliSecond wait.

	self assert: returned.
]

{ #category : #tests }
TFCallbacksTest >> testReentrantCalloutsDuringCallback [

	| callback fun returnValue |
	"Avoid running this test before the image side support handles this case.
	Otherwise both the UI thread and the callback management thread will get blocked in a deadlock."
	"true ifTrue: [ ^ self skip ]."
	
	fun := TFExternalFunction
		name: 'singleCallToCallback'
		moduleName: self libraryPath
		definition: (TFFunctionDefinition
			parameterTypes: {TFBasicType pointer. TFBasicType sint32}
			returnType: TFBasicType sint32).

	callback := TFCallback 
		forCallback: [ :times | 
			times = 7
				ifTrue: [ times ]
				ifFalse: [ runner invokeFunction: fun withArguments: {callback getHandle. times + 1} ] ] 
		parameters: { TFBasicType sint32. } 
		returnType: TFBasicType sint32
		runner: runner.

	returnValue := runner invokeFunction: fun withArguments: {callback getHandle. 0}.
	self assert: returnValue equals: 7
]

{ #category : #tests }
TFCallbacksTest >> testReentrantCalloutsDuringCallbackUsingSameProcessForCallbacks [

	| callback fun returnValue |
	"Avoid running this test before the image side support handles this case.
	Otherwise both the UI thread and the callback management thread will get blocked in a deadlock."
	"true ifTrue: [ ^ self skip ]."
	
	fun := TFExternalFunction
		name: 'singleCallToCallback'
		moduleName: self libraryPath
		definition: (TFFunctionDefinition
			parameterTypes: {TFBasicType pointer. TFBasicType sint}
			returnType: TFBasicType sint).

	callback := TFCallback 
		forCallback: [ :times | 
			times = 7
				ifTrue: [ times ]
				ifFalse: [ 
					runner invokeFunction: fun withArguments: {callback getHandle. times + 1} ] ] 
		parameters: { TFBasicType sint. } 
		returnType: TFBasicType sint
		runner: runner.

	callback runStrategy: TFCallbackSameProcessRunStrategy uniqueInstance.

	returnValue := runner invokeFunction: fun withArguments: {callback getHandle. 0}.
	self assert: returnValue equals: 7
]

{ #category : #tests }
TFCallbacksTest >> testSingleCalloutDuringCallback [

	| callback fun returnValue |

	callback := TFCallback 
		forCallback: [ :a | self shortCallout ] 
		parameters: { TFBasicType sint. } 
		returnType: TFBasicType sint
		runner: runner.

	fun := TFExternalFunction
		name: 'singleCallToCallback'
		moduleName: self libraryPath
		definition: (TFFunctionDefinition
			parameterTypes: {TFBasicType pointer. TFBasicType sint}
			returnType: TFBasicType sint).

	returnValue := runner invokeFunction: fun withArguments: {callback getHandle. 3}.
	self assert: returnValue equals: 42
]

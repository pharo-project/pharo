"
SUnit tests for exceptions

Some of these tests are of the form:
`self assertSuccess: (ExceptionTester new runTest: <<<aSelector>>> )`
For example:
	The code of ExceptionTests>>#testDoubleOuterPass is `self assertSuccess: (ExceptionTester new runTest: #doubleOuterPassTest )`.
	In this example, <<<aSelector>>> = #doubleOuterPassTest.

Here is how these tests are performed:
- ExceptionTester has instance variables 'log' and 'suiteLog' of type OrderedCollection
- ExceptionTester>>#runTest: adds <<<aSelector>>> as a string to 'suiteLog' and clears 'log'
- ExceptionTester>>#runTest: performs the method named <<<aSelector>>>.
  <<<aSelector>>> has an exception-based control flow, and calls methods like #doSomething, #doSomethingElse etc... at some points of it. These methods just add specific strings to 'log' to build a sort of trace of where the execution went.
- If a MyTestError exception would leave method <<<aSelector>>> uncaught, it is caught, the incident is added to 'log', and <<<aSelector>>>'s execution is stopped
- After performing <<<aSelector>>>, ExceptionTester>>#runTest: performs its twin method: <<<aSelector>>>Results (In the example from above, it would be #doubleOuterPassTestResults). This twin method builds the expected value of 'log' by creating an OrderedCollection and filling it with the  right specific strings in the expected order.
- ExceptionTester>>#runTest: compares 'log' and the expected value of 'log'. If they are the same, it appends the 'succeeded' string to the '<<<aSelector>>>' string that was stored in 'suiteLog' earlier. If they are different, it appends the 'failed' string instead.
- Finally, ExceptionTests>>#assertSuccess: asserts that the first element of 'suiteLog' ends with the 'suceeded' string


"
Class {
	#name : #ExceptionTest,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Exception'
}

{ #category : #private }
ExceptionTest >> assertSuccess: anExceptionTester [
	self should: [ ( anExceptionTester suiteLog first) endsWith:  'succeeded'].
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testDoubleOuterPass [
	self assertSuccess: (ExceptionTester new runTest: #doubleOuterPassTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testDoublePassOuter [
	self assertSuccess: (ExceptionTester new runTest: #doublePassOuterTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testDoubleResume [
	self assertSuccess: (ExceptionTester new runTest: #doubleResumeTest ) 
]

{ #category : #'tests - handling' }
ExceptionTest >> testHandlerContext [
	"A test ensuring that when evaluating the action block the exception environment is set to the handler context."

	| result |
	result := [ 
	[ 
	[ MyResumableTestError signal ]
		on: MyTestError
		do: [ 'handler 2' ] ]
		on: MyResumableTestError
		do: [ MyTestError signal ] ]
		on: MyTestError
		do: [ 'handler 1' ].
	self assert: 'handler 1' = result description: 'Incorrect handler'
]

{ #category : #'tests - handling' }
ExceptionTest >> testHandlerFromAction [
	"A test ensuring that nested exceptions work as expected."

	| result |
	result := [ 
	[ 
	[ self error: 'trigger error' ]
		on: ZeroDivide
		do: [ :ex | 'inner' ] ]
		on: Error
		do: [ :ex | 3 / 0 ] ]
		on: ZeroDivide
		do: [ :ex | 'outer' ].
	self assert: 'outer' = result description: 'Incorrect handler'
]

{ #category : #'tests - handling' }
ExceptionTest >> testHandlingExceptionSetWithExclusion [

	| wasHandled  |

	wasHandled := false.

	self
		should: [ 
			[ ZeroDivide signalWithDividend: 1 ]
				on: Error, ArithmeticError - ZeroDivide
				do: [ :exception | 
					wasHandled := true.
					exception return ] ]
		raise: ZeroDivide.
	
	self deny: wasHandled
]

{ #category : #'tests - handling' }
ExceptionTest >> testHandlingWhenThereIsSomeExclusionButDontApplies [

	| wasHandled result |

	wasHandled := false.

	result := [ 
	ZeroDivide signalWithDividend: 1.
	2 ]
		on: Error - MessageNotUnderstood
		do: [ :exception | 
			wasHandled := true.
			exception return ].

	self
		assert: wasHandled;
		assert: result isNil
]

{ #category : #'tests - handling' }
ExceptionTest >> testHandlingWithExclusion [

	| wasHandled  |

	wasHandled := false.

	self
		should: [ 
			[ ZeroDivide signalWithDividend: 1 ]
				on: Error - ZeroDivide
				do: [ :exception | 
					wasHandled := true.
					exception return ] ]
		raise: ZeroDivide.
	
	self deny: wasHandled
]

{ #category : #'tests - handling' }
ExceptionTest >> testHandlingWithSeveralExclusions [
	| wasHandled |
	wasHandled := false.
	self
		should: [ 
			[ ZeroDivide signalWithDividend: 1 ]
				on: Error - Warning - ZeroDivide
				do: [ :exception | 
					wasHandled := true.
					exception return ] ]
		raise: ZeroDivide.
	self deny: wasHandled.
	
	self
		should: [ 
			[ ZeroDivide signalWithDividend: 1 ]
				on: Error - (Warning , ZeroDivide)
				do: [ :exception | 
					wasHandled := true.
					exception return ] ]
		raise: ZeroDivide.
	self deny: wasHandled.
]

{ #category : #'tests - handling' }
ExceptionTest >> testHandlingWithSeveralExclusionsAndExceptionSetsHandling [

	| wasHandled result |

	wasHandled := false.

	result := [ 
	ZeroDivide signalWithDividend: 1.
	2 ]
		on: Error - MessageNotUnderstood - Warning
		do: [ :exception | 
			wasHandled := true.
			exception return ].

	self
		assert: wasHandled;
		assert: result isNil.
		
	wasHandled := false.

	result := [ 
	ZeroDivide signalWithDividend: 1.
	2 ]
		on: Error - (MessageNotUnderstood , Warning)
		do: [ :exception | 
			wasHandled := true.
			exception return ].

	self
		assert: wasHandled;
		assert: result isNil.	
]

{ #category : #tests }
ExceptionTest >> testNoTimeout [
	self assertSuccess: (ExceptionTester new runTest: #simpleNoTimeoutTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testNonResumableFallOffTheEndHandler [
	self assertSuccess: (ExceptionTester new runTest: #nonResumableFallOffTheEndHandler ) 
]

{ #category : #'tests - outer' }
ExceptionTest >> testNonResumableOuter [

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex outer. ex return: 5]
		] raise: Error

]

{ #category : #'tests - outer' }
ExceptionTest >> testNonResumablePass [

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex pass. ex return: 5]
		] raise: Error

]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testResumableFallOffTheEndHandler [
	self assertSuccess: (ExceptionTester new runTest: #resumableFallOffTheEndHandler ) 
]

{ #category : #'tests - outer' }
ExceptionTest >> testResumableOuter [

	| result |
	result := [Notification signal. 4] 
		on: Notification 
		do: [:ex | ex outer. ex return: 5].
	self assert: result equals: 5

]

{ #category : #'tests - outer' }
ExceptionTest >> testResumablePass [
	| result |
	result := [ Notification signal.
	4 ]
		on: Notification
		do: [ :ex | 
			ex pass.
			ex return: 5 ].
	self assert: result equals: 4
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testSignalFromHandlerActionTest [
	self assertSuccess: (ExceptionTester new runTest: #signalFromHandlerActionTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testSignalWithTag [
	| aTag |
	aTag := Object new.
	
	[ 
		DomainError signal: 'aMessage' withTag: aTag.
		self fail: 'The exception was not signaled'. 
	] on: DomainError do: [ :e | 
		self assert: e messageText equals: 'aMessage'.
		self assert: e tag equals: aTag.
	]
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testSimpleEnsure [
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testSimpleEnsureTestWithError [
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithError ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testSimpleEnsureTestWithNotification [
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithNotification ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testSimpleEnsureTestWithUparrow [
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithUparrow ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testSimpleIsNested [
	self assertSuccess: (ExceptionTester new runTest: #simpleIsNestedTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testSimpleOuter [
	self assertSuccess: (ExceptionTester new runTest: #simpleOuterTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testSimplePass [
	self assertSuccess: (ExceptionTester new runTest: #simplePassTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testSimpleResignalAs [
	self assertSuccess: (ExceptionTester new runTest: #simpleResignalAsTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testSimpleResume [
	self assertSuccess: (ExceptionTester new runTest: #simpleResumeTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTest >> testSimpleReturn [
	self assertSuccess: (ExceptionTester new runTest: #simpleReturnTest ) 
]

{ #category : #tests }
ExceptionTest >> testTimeoutWithZeroDuration [
	self assertSuccess: (ExceptionTester new runTest: #simpleTimeoutWithZeroDurationTest ) 
]

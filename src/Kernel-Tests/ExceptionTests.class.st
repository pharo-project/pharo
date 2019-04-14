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
	#name : #ExceptionTests,
	#superclass : #TestCase,
	#category : #'Kernel-Tests-Exception'
}

{ #category : #private }
ExceptionTests >> assertSuccess: anExceptionTester [
	self should: [ ( anExceptionTester suiteLog first) endsWith:  'succeeded'].
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testDoubleOuterPass [
	self assertSuccess: (ExceptionTester new runTest: #doubleOuterPassTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testDoublePassOuter [
	self assertSuccess: (ExceptionTester new runTest: #doublePassOuterTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testDoubleResume [
	self assertSuccess: (ExceptionTester new runTest: #doubleResumeTest ) 
]

{ #category : #'tests - handling' }
ExceptionTests >> testHandlerContext [
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
ExceptionTests >> testHandlerFromAction [
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
ExceptionTests >> testHandlingExceptionSetWithExclusion [

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
ExceptionTests >> testHandlingWhenThereIsSomeExclusionButDontApplies [

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
ExceptionTests >> testHandlingWithExclusion [

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
ExceptionTests >> testHandlingWithSeveralExclusions [
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
ExceptionTests >> testHandlingWithSeveralExclusionsAndExceptionSetsHandling [

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
ExceptionTests >> testNoTimeout [
	self assertSuccess: (ExceptionTester new runTest: #simpleNoTimeoutTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testNonResumableFallOffTheEndHandler [
	self assertSuccess: (ExceptionTester new runTest: #nonResumableFallOffTheEndHandler ) 
]

{ #category : #'tests - outer' }
ExceptionTests >> testNonResumableOuter [

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex outer. ex return: 5]
		] raise: Error

]

{ #category : #'tests - outer' }
ExceptionTests >> testNonResumablePass [

	self should: [
		[Error signal. 4] 
			on: Error 
			do: [:ex | ex pass. ex return: 5]
		] raise: Error

]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testResumableFallOffTheEndHandler [
	self assertSuccess: (ExceptionTester new runTest: #resumableFallOffTheEndHandler ) 
]

{ #category : #'tests - outer' }
ExceptionTests >> testResumableOuter [

	| result |
	result := [Notification signal. 4] 
		on: Notification 
		do: [:ex | ex outer. ex return: 5].
	self assert: result = 5

]

{ #category : #'tests - outer' }
ExceptionTests >> testResumablePass [

	| result |
	result := [Notification signal. 4] 
		on: Notification 
		do: [:ex | ex pass. ex return: 5].
	self assert: result = 4

]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testSignalFromHandlerActionTest [
	self assertSuccess: (ExceptionTester new runTest: #signalFromHandlerActionTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testSignalWithTag [
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
ExceptionTests >> testSimpleEnsure [
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testSimpleEnsureTestWithError [
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithError ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testSimpleEnsureTestWithNotification [
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithNotification ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testSimpleEnsureTestWithUparrow [
	self assertSuccess: (ExceptionTester new runTest: #simpleEnsureTestWithUparrow ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testSimpleIsNested [
	self assertSuccess: (ExceptionTester new runTest: #simpleIsNestedTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testSimpleOuter [
	self assertSuccess: (ExceptionTester new runTest: #simpleOuterTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testSimplePass [
	self assertSuccess: (ExceptionTester new runTest: #simplePassTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testSimpleResignalAs [
	self assertSuccess: (ExceptionTester new runTest: #simpleResignalAsTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testSimpleResume [
	self assertSuccess: (ExceptionTester new runTest: #simpleResumeTest ) 
]

{ #category : #'tests - exceptiontester' }
ExceptionTests >> testSimpleReturn [
	self assertSuccess: (ExceptionTester new runTest: #simpleReturnTest ) 
]

{ #category : #tests }
ExceptionTests >> testTimeoutWithZeroDuration [
	self assertSuccess: (ExceptionTester new runTest: #simpleTimeoutWithZeroDurationTest ) 
]

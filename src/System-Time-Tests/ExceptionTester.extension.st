Extension { #name : 'ExceptionTester' }

{ #category : '*System-Time-Tests' }
ExceptionTester >> simpleNoTimeoutTest [

	[ self doSomething ]
		valueWithinMilliseconds: 86400000 "1 day milliseconds" onTimeout:
			[ self doSomethingElse ]
]

{ #category : '*System-Time-Tests' }
ExceptionTester >> simpleNoTimeoutTestResults [

	^OrderedCollection new
		add: self doSomethingString;
		yourself
]

{ #category : '*System-Time-Tests' }
ExceptionTester >> simpleTimeoutTest [

	| n |
	[
	1 to: 1000000 do: [ :i |
		n := i.
		self doSomething ] ] valueWithinMilliseconds: 50 onTimeout: [
		self iterationsBeforeTimeout: n.
		self doSomethingElse ]
]

{ #category : '*System-Time-Tests' }
ExceptionTester >> simpleTimeoutTestResults [

	| things |
	things := OrderedCollection new: self iterationsBeforeTimeout.

	self iterationsBeforeTimeout timesRepeat: [ things add: self  doSomethingString ].
	things add: self doSomethingElseString.

	^ things
]

{ #category : '*System-Time-Tests' }
ExceptionTester >> simpleTimeoutWithZeroDurationTest [

	[ self doSomething ]
		valueWithinMilliseconds: 0
		onTimeout: [ self doSomethingElse ]
]

{ #category : '*System-Time-Tests' }
ExceptionTester >> simpleTimeoutWithZeroDurationTestResults [

	^OrderedCollection new
		add: self doSomethingElseString;
		yourself
]

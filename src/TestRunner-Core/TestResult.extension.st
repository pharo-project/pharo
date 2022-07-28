Extension { #name : #TestResult }

{ #category : #'*TestRunner-Core' }
TestResult >> + aTestResult [

	self cleanMergeWith: aTestResult
]

{ #category : #'*TestRunner-Core' }
TestResult >> addErrorIfAbsent: anError [

	(errors includes: anError) ifFalse: [ self addError: anError ]
]

{ #category : #'*TestRunner-Core' }
TestResult >> addFailureIfAbsent: aFailure [

	(failures includes: aFailure) ifFalse: [ self addFailure: aFailure ]
]

{ #category : #'*TestRunner-Core' }
TestResult >> addPassIfAbsent: aPassed [

	(passed includes: aPassed) ifFalse: [ self addPass: aPassed ]
]

{ #category : #'*TestRunner-Core' }
TestResult >> addSkipIfAbsent: aSkip [

	(skipped includes: aSkip) ifFalse: [ self addSkip: aSkip ]
]

{ #category : #'*TestRunner-Core' }
TestResult >> cleanMergeWith: aTestResult [
	timeStamp := aTestResult timeStamp.
	aTestResult actualPassed do: [ :each | self addPassIfAbsent: each ].
	aTestResult actualFailures do: [ :each | self addFailureIfAbsent: each ].
	aTestResult actualErrors do: [ :each | self addErrorIfAbsent: each ].
	aTestResult skipped do: [ :each | self addSkipIfAbsent: each ]
]

{ #category : #'*TestRunner-Core' }
TestResult >> removeError: aTestCase [

	"remove the giving error from the errors set"

	^ errors remove: aTestCase
]

{ #category : #'*TestRunner-Core' }
TestResult >> removeFailure: aTestCase [

	"remove the giving failure from the failures set"

	^ failures remove: aTestCase
]

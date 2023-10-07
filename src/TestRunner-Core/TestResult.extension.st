Extension { #name : 'TestResult' }

{ #category : '*TestRunner-Core' }
TestResult >> + aTestResult [

	aTestResult passed do: [ :each | self addPassIfAbsent: each ].
	aTestResult failures do: [ :each | self addFailureIfAbsent: each ].
	aTestResult errors do: [ :each | self addErrorIfAbsent: each ].
	aTestResult skipped do: [ :each | self addSkipIfAbsent: each ]
]

{ #category : '*TestRunner-Core' }
TestResult >> addErrorIfAbsent: anError [

	(errors includes: anError) ifFalse: [ self addError: anError ]
]

{ #category : '*TestRunner-Core' }
TestResult >> addFailureIfAbsent: aFailure [

	(failures includes: aFailure) ifFalse: [ self addFailure: aFailure ]
]

{ #category : '*TestRunner-Core' }
TestResult >> addPassIfAbsent: aPassed [

	(passed includes: aPassed) ifFalse: [ self addPass: aPassed ]
]

{ #category : '*TestRunner-Core' }
TestResult >> addSkipIfAbsent: aSkip [

	(skipped includes: aSkip) ifFalse: [ self addSkip: aSkip ]
]

{ #category : '*TestRunner-Core' }
TestResult >> removeError: aTestCase [

	"remove the giving error from the errors set"

	^ errors remove: aTestCase
]

{ #category : '*TestRunner-Core' }
TestResult >> removeFailure: aTestCase [

	"remove the giving failure from the failures set"

	^ failures remove: aTestCase
]

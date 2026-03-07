"
I am a `TestResult` for `NewTestRunner` 
"
Class {
	#name : 'TestRunnerResult',
	#superclass : 'TestResult',
	#category : 'TestRunner-Core-Core',
	#package : 'TestRunner-Core',
	#tag : 'Core'
}

{ #category : 'combining' }
TestRunnerResult >> mergeWith: aTestResult [
	"merge the given test result with the receiver"

	timeStamp := DateAndTime now.
	failures addAll: (aTestResult failures difference: failures).
	errors addAll: (aTestResult errors difference: errors).
	passed addAll: (aTestResult passed difference: passed).
	skipped addAll: (aTestResult skipped difference: skipped)
]

{ #category : 'removing' }
TestRunnerResult >> removeError: aTestCase [
	"remove the giving error from the errors set"

	^ errors remove: aTestCase
]

{ #category : 'removing' }
TestRunnerResult >> removeFailure: aTestCase [
	"remove the giving failure from the failures set"

	^ failures remove: aTestCase
]

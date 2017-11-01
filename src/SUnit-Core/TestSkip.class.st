"
I mark a test as being skipped.
"
Class {
	#name : #TestSkip,
	#superclass : #Exception,
	#category : 'SUnit-Core-Extensions'
}

{ #category : #'exception handling' }
TestSkip >> defaultAction [
	"nothing to do.
	If I am not handled I might trigger an Assertion failure later on."
]

{ #category : #'exception handling' }
TestSkip >> sunitAnnounce: aTestCase toResult: aTestResult [
	aTestResult addSkip: aTestCase.
]

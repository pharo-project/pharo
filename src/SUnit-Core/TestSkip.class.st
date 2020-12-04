"
I mark a test as being skipped.
"
Class {
	#name : #TestSkip,
	#superclass : #Exception,
	#category : #'SUnit-Core-Extensions'
}

{ #category : #'exception handling' }
TestSkip >> defaultAction [
	"nothing to do.
	If I am not handled I might trigger an Assertion failure later on."
]

{ #category : #'exception handling' }
TestSkip >> manageTestProcessBy: aProcessMonitorTestService [
	"it is special exception which do not represent the actual test failure.
	So we just ignoring it"
]

{ #category : #'exception handling' }
TestSkip >> sunitAnnounce: aTestCase toResult: aTestResult [
	aTestResult addSkip: aTestCase.
]

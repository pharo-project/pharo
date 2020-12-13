"
I mark a test as being skipped.
"
Class {
	#name : #TestSkipped,
	#superclass : #Exception,
	#category : #'SUnit-Core-Extensions'
}

{ #category : #'exception handling' }
TestSkipped >> defaultAction [
	"nothing to do.
	If I am not handled I might trigger an Assertion failure later on."
]

{ #category : #'exception handling' }
TestSkipped >> manageTestProcessBy: aProcessMonitorTestService [
	"it is special exception which do not represent the actual test failure.
	So we just ignoring it"
]

{ #category : #'exception handling' }
TestSkipped >> sunitAnnounce: aTestCase toResult: aTestResult [
	aTestResult addSkip: aTestCase.
]

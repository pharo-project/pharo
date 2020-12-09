"
I mark a test as being skipped.
"
Class {
	#name : #SkippingTest,
	#superclass : #Exception,
	#category : #'SUnit-Core-Extensions'
}

{ #category : #'exception handling' }
SkippingTest >> defaultAction [
	"nothing to do.
	If I am not handled I might trigger an Assertion failure later on."
]

{ #category : #'exception handling' }
SkippingTest >> manageTestProcessBy: aProcessMonitorTestService [
	"it is special exception which do not represent the actual test failure.
	So we just ignoring it"
]

{ #category : #'exception handling' }
SkippingTest >> sunitAnnounce: aTestCase toResult: aTestResult [
	aTestResult addSkip: aTestCase.
]

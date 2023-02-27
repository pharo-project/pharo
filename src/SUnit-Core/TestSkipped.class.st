"
I mark a test as being skipped.
"
Class {
	#name : #TestSkipped,
	#superclass : #Exception,
	#category : #'SUnit-Core-Extensions'
}

{ #category : #handling }
TestSkipped >> defaultAction [
	"Nothing to do. If I am not handled I might trigger an Assertion failure later on."
]

{ #category : #handling }
TestSkipped >> manageTestProcessBy: aProcessMonitorTestService [
	"It is special exception which do not represent the actual test failure.
	 So we just ignoring it"
]

{ #category : #handling }
TestSkipped >> sunitAnnounce: aTestCase toResult: aTestResult [

	aTestResult addSkip: aTestCase
]

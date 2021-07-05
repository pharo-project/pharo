"
This announcement is thrown when a test suite just finished (more precisely when hisoties are updated)
"
Class {
	#name : #TestSuiteEnded,
	#superclass : #TestCaseAnnouncement,
	#category : #'SUnit-Core-Announcements'
}

{ #category : #'instance creation' }
TestSuiteEnded class >> result: aResult [

	^ self new
		testResult: aResult;
		yourself
]

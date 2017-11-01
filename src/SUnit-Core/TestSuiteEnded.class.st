"
This announcment is thrown when a test suite just finished (more precisly when hisoties are updated)
"
Class {
	#name : #TestSuiteEnded,
	#superclass : #TestCaseAnnouncement,
	#category : 'SUnit-Core-Announcements'
}

{ #category : #'instance creation' }
TestSuiteEnded class >> result: aResult [

	^ self new
		testResult: aResult;
		yourself
]

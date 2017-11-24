"
Notify about testing a test
"
Class {
	#name : #TestAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'test'
	],
	#category : #'SUnit-Core-Kernel'
}

{ #category : #'instance creation' }
TestAnnouncement class >> with: aTestCase [
	^self new
		test: aTestCase;
		yourself
]

{ #category : #accessing }
TestAnnouncement >> test [
	^test
]

{ #category : #accessing }
TestAnnouncement >> test: aTest [
	test := aTest
]

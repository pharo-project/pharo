"
We will later separate the tests from the package.
"
Class {
	#name : #RecentMessageListTest,
	#superclass : #TestCase,
	#category : #'RecentSubmissions-Core'
}

{ #category : #tests }
RecentMessageListTest >> testRegisteredRegister [

	| wasAlreadyOn |
	wasAlreadyOn := (SystemAnnouncer uniqueInstance hasSubscriber: RecentMessageList uniqueInstance).
	
	RecentMessageList startReceivingNotifications.
	self assert: (SystemAnnouncer uniqueInstance hasSubscriber: RecentMessageList uniqueInstance).
	
	RecentMessageList stopReceivingNotifications.
	self deny: (SystemAnnouncer uniqueInstance hasSubscriber: RecentMessageList uniqueInstance).
	
	wasAlreadyOn ifTrue: [ RecentMessageList startReceivingNotifications ] 
]

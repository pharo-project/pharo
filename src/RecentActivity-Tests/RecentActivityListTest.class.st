"
We will later separate the tests from the package.
"
Class {
	#name : #RecentActivityListTest,
	#superclass : #TestCase,
	#category : #'RecentActivity-Tests'
}

{ #category : #tests }
RecentActivityListTest >> testRegisteredRegister [

	| wasAlreadyOn |
	wasAlreadyOn := (SystemAnnouncer uniqueInstance hasSubscriber: RecentActivityList uniqueInstance).
	
	RecentActivityList startReceivingNotifications.
	self assert: (SystemAnnouncer uniqueInstance hasSubscriber: RecentActivityList uniqueInstance).
	
	RecentActivityList stopReceivingNotifications.
	self deny: (SystemAnnouncer uniqueInstance hasSubscriber: RecentActivityList uniqueInstance).
	
	wasAlreadyOn ifTrue: [ RecentActivityList startReceivingNotifications ] 
]

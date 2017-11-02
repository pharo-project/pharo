"
I'm an abstract superclass for Job announcements. 

My subclasses are announced in Job jobAnnouncer.
"
Class {
	#name : #JobAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'job'
	],
	#category : 'Jobs-Announcements'
}

{ #category : #'instance creation' }
JobAnnouncement class >> on: aJob [ 

	^ self new job: aJob.
]

{ #category : #accessing }
JobAnnouncement >> job [
	
	^ job
]

{ #category : #accessing }
JobAnnouncement >> job: anObject [
	
	job := anObject
]

{ #category : #accessing }
JobAnnouncement >> max [
	^ job max
]

{ #category : #accessing }
JobAnnouncement >> min [
	^ job min
]

{ #category : #accessing }
JobAnnouncement >> progress [
	^ job progress
]

{ #category : #accessing }
JobAnnouncement >> title [
	^ job title
]

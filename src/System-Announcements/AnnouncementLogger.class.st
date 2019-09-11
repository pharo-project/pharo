"
Use me to debug and log to the transcript annoncements.


AnnouncementLogger new subscribeTo: SystemAnnouncer announcer

To unsubscribe

AnnouncementLogger  allInstancesDo: [:each | SystemAnnouncer announcer unsubscribe: each ]
"
Class {
	#name : #AnnouncementLogger,
	#superclass : #Object,
	#category : #'System-Announcements-Logging'
}

{ #category : #logging }
AnnouncementLogger >> logAnnouncement: ann [
	ann traceCr
]

{ #category : #subscription }
AnnouncementLogger >> subscribeTo: anAnnouncer [

	anAnnouncer "weak" 
		when: Announcement send: #logAnnouncement: to: self
]

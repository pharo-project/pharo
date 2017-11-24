"
A JobChange is an announcement for a job change. To get notify

SystemAnnouncer uniqueInstance 
	on: JobChange send: #XXX to: whoever.
"
Class {
	#name : #JobChange,
	#superclass : #JobAnnouncement,
	#category : #'Jobs-Announcements'
}

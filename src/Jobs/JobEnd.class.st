"
A JobChange is an announcement for a job end.

A JobChange is an announcement for a job change. To get notify

SystemAnnouncer uniqueInstance 
	on: JobEnd send: #XXX to: whoever.
"
Class {
	#name : 'JobEnd',
	#superclass : 'JobAnnouncement',
	#category : 'Jobs-Announcements',
	#package : 'Jobs',
	#tag : 'Announcements'
}

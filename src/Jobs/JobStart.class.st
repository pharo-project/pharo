"
A JobStart is announcement announcing the start of a job execution.

SystemAnnouncer uniqueInstance 
	on: JobStart send: #XXX to: whoever.
"
Class {
	#name : #JobStart,
	#superclass : #JobAnnouncement,
	#category : #Jobs-Announcements
}

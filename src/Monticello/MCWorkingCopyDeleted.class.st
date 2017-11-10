"
A MCWorkingCopyDeleted class is an announcement raised when a MCWorkingCopy is removed
"
Class {
	#name : #MCWorkingCopyDeleted,
	#superclass : #Announcement,
	#instVars : [
		'package',
		'workingCopy'
	],
	#category : #Monticello-Announcements
}

{ #category : #'instance creation' }
MCWorkingCopyDeleted class >> workingCopy: workingCopy [

	^ self new
		workingCopy: workingCopy;
		yourself
]

{ #category : #'instance creation' }
MCWorkingCopyDeleted class >> workingCopy: aWC package: package [

	^ self new
		workingCopy: aWC;
		package: package;
		yourself
]

{ #category : #accessing }
MCWorkingCopyDeleted >> package [
	^ package
]

{ #category : #accessing }
MCWorkingCopyDeleted >> package: anObject [
	package := anObject
]

{ #category : #accessing }
MCWorkingCopyDeleted >> workingCopy [
	^ workingCopy
]

{ #category : #accessing }
MCWorkingCopyDeleted >> workingCopy: anObject [
	workingCopy := anObject
]

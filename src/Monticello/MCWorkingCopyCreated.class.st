"
A MCWorkingCopyCreated is a announcement raised when a new MCWorkingCopy is created
"
Class {
	#name : #MCWorkingCopyCreated,
	#superclass : #Announcement,
	#instVars : [
		'package',
		'workingCopy'
	],
	#category : 'Monticello-Announcements'
}

{ #category : #'instance creation' }
MCWorkingCopyCreated class >> workingCopy: workingCopy [

	^ self new
		workingCopy: workingCopy;
		yourself
]

{ #category : #'instance creation' }
MCWorkingCopyCreated class >> workingCopy: aWC package: package [

	^ self new
		workingCopy: aWC;
		package: package;
		yourself
]

{ #category : #accessing }
MCWorkingCopyCreated >> package [
	^ package
]

{ #category : #accessing }
MCWorkingCopyCreated >> package: anObject [
	package := anObject
]

{ #category : #accessing }
MCWorkingCopyCreated >> workingCopy [
	^ workingCopy
]

{ #category : #accessing }
MCWorkingCopyCreated >> workingCopy: anObject [
	workingCopy := anObject
]

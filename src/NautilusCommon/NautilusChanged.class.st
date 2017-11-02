"
A NautilusChanged is raised when a NautilusUI send th message #changed:
"
Class {
	#name : #NautilusChanged,
	#superclass : #Announcement,
	#instVars : [
		'symbol'
	],
	#category : 'NautilusCommon-Announcements'
}

{ #category : #'instance creation' }
NautilusChanged class >> symbol: symbol [

	^ self new
		symbol: symbol;
		yourself
]

{ #category : #accessing }
NautilusChanged >> symbol [
	^ symbol
]

{ #category : #accessing }
NautilusChanged >> symbol: anObject [
	symbol := anObject
]

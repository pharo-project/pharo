"
A NautilusTextDisplayerChanged is raised when the way to display the method changed.
"
Class {
	#name : #NautilusTextDisplayerChanged,
	#superclass : #Announcement,
	#instVars : [
		'displayerSymbol'
	],
	#category : #NautilusCommon-Announcements
}

{ #category : #'instance creation' }
NautilusTextDisplayerChanged class >> displayerSymbol: anObject [

	^ self new
		displayerSymbol: anObject;
		yourself
]

{ #category : #accessing }
NautilusTextDisplayerChanged >> displayerSymbol [
	^ displayerSymbol
]

{ #category : #accessing }
NautilusTextDisplayerChanged >> displayerSymbol: anObject [
	displayerSymbol := anObject
]

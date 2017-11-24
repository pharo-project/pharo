"
Announce that the current change set has changed
"
Class {
	#name : #CurrentChangeSetChanged,
	#superclass : #Announcement,
	#instVars : [
		'new',
		'old'
	],
	#category : #'System-Changes'
}

{ #category : #accessing }
CurrentChangeSetChanged >> new [
	^ new
]

{ #category : #accessing }
CurrentChangeSetChanged >> new: anObject [
	new := anObject
]

{ #category : #accessing }
CurrentChangeSetChanged >> old [
	^ old
]

{ #category : #accessing }
CurrentChangeSetChanged >> old: anObject [
	old := anObject  
]

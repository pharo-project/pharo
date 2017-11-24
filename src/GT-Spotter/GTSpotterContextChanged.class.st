Class {
	#name : #GTSpotterContextChanged,
	#superclass : #Announcement,
	#instVars : [
		'previous',
		'current'
	],
	#category : #'GT-Spotter-Announcements'
}

{ #category : #compatibility }
GTSpotterContextChanged >> context [
	^ self current
]

{ #category : #compatibility }
GTSpotterContextChanged >> context: aSpotterContext [
	self current: aSpotterContext
]

{ #category : #accessing }
GTSpotterContextChanged >> current [
	^ current
]

{ #category : #accessing }
GTSpotterContextChanged >> current: anObject [
	current := anObject
]

{ #category : #'accessing-dynamic' }
GTSpotterContextChanged >> currentText [
	^ self current ifNil: [''] ifNotNil: [ self current text ]
]

{ #category : #accessing }
GTSpotterContextChanged >> previous [
	^ previous
]

{ #category : #accessing }
GTSpotterContextChanged >> previous: anObject [
	previous := anObject
]

{ #category : #'accessing-dynamic' }
GTSpotterContextChanged >> previousText [
	^ self previous ifNil: ['']  ifNotNil: [ self previous text ]
]

{ #category : #compatibility }
GTSpotterContextChanged >> text [
	^ self currentText
]

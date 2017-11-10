"
Announcement sent when a Version is starting to load
"
Class {
	#name : #MCVersionLoaderStarted,
	#superclass : #Announcement,
	#instVars : [
		'versionLoader',
		'label'
	],
	#category : #Monticello-Announcements
}

{ #category : #accessing }
MCVersionLoaderStarted >> label [
	^ label
]

{ #category : #accessing }
MCVersionLoaderStarted >> label: anObject [
	label := anObject
]

{ #category : #accessing }
MCVersionLoaderStarted >> versionLoader [
	^ versionLoader
]

{ #category : #accessing }
MCVersionLoaderStarted >> versionLoader: anObject [
	versionLoader := anObject
]

"
Announcement sent when a Version has finished loading
"
Class {
	#name : #MCVersionLoaderStopped,
	#superclass : #Announcement,
	#instVars : [
		'label',
		'versionLoader'
	],
	#category : #'Monticello-Announcements'
}

{ #category : #accessing }
MCVersionLoaderStopped >> label [
	^ label
]

{ #category : #accessing }
MCVersionLoaderStopped >> label: anObject [
	label := anObject
]

{ #category : #accessing }
MCVersionLoaderStopped >> versionLoader [
	^ versionLoader
]

{ #category : #accessing }
MCVersionLoaderStopped >> versionLoader: anObject [
	versionLoader := anObject
]

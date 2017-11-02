Class {
	#name : #MetacelloClearStackCacheNotification,
	#superclass : #Notification,
	#instVars : [
		'cacheNames'
	],
	#category : 'Metacello-Core-Exceptions-Notifications'
}

{ #category : #accessing }
MetacelloClearStackCacheNotification >> cacheNames [
	^ cacheNames
]

{ #category : #accessing }
MetacelloClearStackCacheNotification >> cacheNames: anArray [
	cacheNames := anArray
]

{ #category : #signaling }
MetacelloClearStackCacheNotification >> signal: anArray [
	"Signal the occurrence of an exceptional condition with a specified cacheName."

	self cacheNames: anArray.
	^ self signal
]

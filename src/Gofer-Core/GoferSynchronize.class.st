"
Synchronize repos 
"
Class {
	#name : #GoferSynchronize,
	#superclass : #GoferOperation,
	#instVars : [
		'cacheReferences'
	],
	#category : #Gofer-Core
}

{ #category : #accessing }
GoferSynchronize >> cacheRepository [
	^ MCCacheRepository uniqueInstance
]

{ #category : #initialization }
GoferSynchronize >> initializeOn: aGofer [
	super initializeOn: aGofer disablePackageCache.
	MCFileBasedRepository flushAllCaches.
	cacheReferences := self gofer allResolvedIn: self cacheRepository
]

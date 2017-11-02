"
Fetch packages using Gofer
"
Class {
	#name : #GoferFetch,
	#superclass : #GoferSynchronize,
	#category : 'Gofer-Core'
}

{ #category : #private }
GoferFetch >> defaultModel [
	^ Set new
]

{ #category : #running }
GoferFetch >> execute [
	self model
		do: [ :reference | self cacheRepository storeVersion: reference version ]
		displayingProgress: 'Fetching Versions'
]

{ #category : #initialization }
GoferFetch >> initializeOn: aGofer [
	super initializeOn: aGofer.
	self gofer references do: [ :reference |
		self gofer allResolved do: [ :resolved |
			((reference matches: resolved) and: [ (cacheReferences includes: resolved) not ])
				ifTrue: [ self model add: resolved ] ] ]
]

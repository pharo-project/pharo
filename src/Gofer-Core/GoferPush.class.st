"
Push a version
"
Class {
	#name : #GoferPush,
	#superclass : #GoferSynchronize,
	#category : 'Gofer-Core'
}

{ #category : #private }
GoferPush >> defaultModel [
	^ OrderedCollection new
]

{ #category : #running }
GoferPush >> execute [
	self model 
		do: [ :assocation | assocation value storeVersion: assocation key version ]
		displayingProgress: 'Pushing Versions'
]

{ #category : #initialization }
GoferPush >> initializeOn: aGofer [
	super initializeOn: aGofer.
	self gofer references do: [ :reference |
		cacheReferences do: [ :resolved |
			(reference matches: resolved) ifTrue: [
				self gofer repositories do: [ :repository |
					((self gofer allResolvedIn: repository) includes: resolved)
						ifFalse: [ self model add: resolved -> repository ] ] ] ] ]
]

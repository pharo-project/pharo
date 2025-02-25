Extension { #name : 'BlockClosure' }

{ #category : '*Jobs' }
BlockClosure >> asJob [
	"the owner is the object where the job was started, not where the block was created"
	^ self asJobWithOwner: thisContext sender receiver
]

{ #category : '*Jobs' }
BlockClosure >> asJobWithOwner: anObject [

	^ Job block: self owner: anObject
]

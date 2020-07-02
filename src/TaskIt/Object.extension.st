Extension { #name : #Object }

{ #category : #'*TaskIt' }
Object >> asWeakReference [
	^ (WeakArray new: 1)
		at: 1 put: self;
		yourself
]

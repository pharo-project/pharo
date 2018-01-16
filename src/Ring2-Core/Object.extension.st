Extension { #name : #Object }

{ #category : #'*Ring2-Core' }
Object >> isRing2FullyResolved [

	^ true
]

{ #category : #'*Ring2-Core' }
Object >> isRing2FullyUnresolved [

	^ false
]

{ #category : #'*Ring2-Core' }
Object >> isRing2Object [

	^ false
]

{ #category : #'*Ring2-Core' }
Object >> isRing2Resolved [

	^ true
]

{ #category : #'*Ring2-Core' }
Object >> orDefaultForUnresolved [

	^ self
]

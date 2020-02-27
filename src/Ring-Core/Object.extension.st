Extension { #name : #Object }

{ #category : #'*Ring-Core' }
Object >> isRGObject [

	^ false
]

{ #category : #'*Ring-Core' }
Object >> isRGUnresolvedValue [

	^ false
]

{ #category : #'*Ring-Core' }
Object >> isRingFullyResolved [

	^ true
]

{ #category : #'*Ring-Core' }
Object >> isRingFullyUnresolved [

	^ false
]

{ #category : #'*Ring-Core' }
Object >> isRingResolved [

	^ true
]

{ #category : #'*Ring-Core' }
Object >> markAsRingResolved [

	"do nothing"
]

{ #category : #'*Ring-Core' }
Object >> orDefaultForUnresolved [

	^ self
]

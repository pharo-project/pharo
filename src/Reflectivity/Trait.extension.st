Extension { #name : #Trait }

{ #category : #'*Reflectivity' }
Trait >> nonAnonymousClass [
	^ self class isAnonymous
		ifTrue: [ self class superclass ]
		ifFalse: [ self class ]
]

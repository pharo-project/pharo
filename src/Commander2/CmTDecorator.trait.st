"
I implement methods that a decorator object understand.
"
Trait {
	#name : #CmTDecorator,
	#category : #'Commander2-Traits'
}

{ #category : #'instance creation' }
CmTDecorator classSide >> decorate: anObject [
	"Create a decorator decorating anObject."
	^ self new
		decorate: anObject;
		yourself
]

{ #category : #decorating }
CmTDecorator >> decorate: anObject [
	"Decorate anObject. To be defined by users of this trait."
	self explicitRequirement
]

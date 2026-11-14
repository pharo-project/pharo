Extension { #name : 'SimpleAnnotationContext' }

{ #category : '*Commander-Calypso-Compatibility' }
SimpleAnnotationContext >> commandAnchor [
	"The anchor of ClyCommandMap that describes the same contexts as me, used to convert class annotations into declarations."

	^ annotationUserClass
]

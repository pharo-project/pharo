Extension { #name : #Object }

{ #category : #'*ClassAnnotation' }
Object >> representsSimpleAnnotationUser: someUserClass [

	^self isKindOf: someUserClass
]

Extension { #name : #RGClassDefinition }

{ #category : #'*NautilusCommon' }
RGClassDefinition >> compile: aString classified: aCategory notifying: aController [

	self realClass compile: aString classified: aCategory notifying: aController
]

{ #category : #'*NautilusCommon' }
RGClassDefinition >> inheritsFrom: aClass [

	^ self realClass inheritsFrom: aClass
]

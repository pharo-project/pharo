Extension { #name : #GTSpotterExtensionSettings }

{ #category : #'*renraku' }
GTSpotterExtensionSettings class >> Object_spotterRePropertiesFor [

	^ ReSettings spotterPlugin 
]

{ #category : #'*renraku' }
GTSpotterExtensionSettings class >> Object_spotterRePropertiesFor: aBoolean [

	^ ReSettings spotterPlugin: aBoolean
]

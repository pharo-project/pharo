Extension { #name : 'UUID' }

{ #category : '*Deprecated14' }
UUID class >> nonStandardUUID [
	"This method uses the legacy UUID generation method from Pharo."
	
	^ self nonStandardUUIDWithGenerator: UUIDGenerator default
]

{ #category : '*Deprecated14' }
UUID class >> nonStandardUUIDWithGenerator: aGenerator [
	"This method uses the legacy UUID generation method from Pharo."
	
	|uuid|
	uuid := super new.
	aGenerator placeFields: uuid uuidData.
	^uuid
]

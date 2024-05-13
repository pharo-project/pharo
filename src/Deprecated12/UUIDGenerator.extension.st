Extension { #name : 'UUIDGenerator' }

{ #category : '*Deprecated12' }
UUIDGenerator class >> next [
	"Return a UUID using the shared generator"

	"self next"

	self
		deprecated:
		'The UUIDGenerator method should not be used directly to generate new UUIDs'
		transformWith: 'UUIDGenerator next' -> 'UUID new'.

	^ UUID new
]

{ #category : '*Deprecated12' }
UUIDGenerator >> next [
	"Return the next UUID, see my class comment"

	self
		deprecated:
		'The UUIDGenerator method should not be used directly to generate new UUIDs'
		transformWith: '`@receiver next' -> 'UUID new'.
	^ UUID new
]

Extension { #name : 'UndefinedObject' }

{ #category : '*UnifiedFFI' }
UndefinedObject >> isNullOrNil [

	^ true
]

{ #category : '*UnifiedFFI' }
UndefinedObject >> packToArity: aNumber [
	"pointer to NULL is NULL too"
	^ self
]

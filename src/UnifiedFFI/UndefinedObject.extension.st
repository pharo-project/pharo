Extension { #name : #UndefinedObject }

{ #category : #'*UnifiedFFI' }
UndefinedObject >> packToArity: aNumber [
	"pointer to NULL is NULL too"
	^ self
]

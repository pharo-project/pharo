Extension { #name : #FFITypeArrayType }

{ #category : #'*ThreadedFFI-UFFI' }
FFITypeArrayType >> tfExternalTypeForStructure [

	^ Array new: self objectClass numberOfElements withAll: self objectClass type tfExternalTypeWithArity
]

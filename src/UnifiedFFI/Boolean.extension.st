Extension { #name : 'Boolean' }

{ #category : '*UnifiedFFI' }
Boolean class >> asExternalTypeOn: generator [
	^ FFIBool asExternalTypeOn: generator
]

{ #category : '*UnifiedFFI' }
Boolean >> asExternalTypeOn: generator [
	^ FFIConst value: self asBit
]

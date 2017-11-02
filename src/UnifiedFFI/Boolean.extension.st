Extension { #name : #Boolean }

{ #category : #'*UnifiedFFI' }
Boolean >> asExternalTypeOn: generator [ 
	^ FFIConst value: self asBit
]

{ #category : #'*UnifiedFFI' }
Boolean class >> asExternalTypeOn: generator [ 
	^ FFIBool asExternalTypeOn: generator
]

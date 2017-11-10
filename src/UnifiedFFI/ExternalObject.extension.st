Extension { #name : #ExternalObject }

{ #category : #'*UnifiedFFI' }
ExternalObject >> autoRelease [
	self isExternalAddress ifFalse: [ ^ self ].
	^ self getHandle autoRelease
]

{ #category : #'*UnifiedFFI' }
ExternalObject >> pointer [ 
	^ self getHandle pointer
]

{ #category : #'*UnifiedFFI' }
ExternalObject >> registerAsExternalResource [
	self deprecated: 'Use #autoRelease instead.' on: '2016-01-22' in: #Pharo5.  
	self isExternalAddress ifFalse: [ ^ self ].
	^ self getHandle registerAsExternalResource
]

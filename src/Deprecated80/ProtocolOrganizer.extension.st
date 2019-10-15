Extension { #name : #ProtocolOrganizer }

{ #category : #'*Deprecated80' }
ProtocolOrganizer >> allMethods [
	self deprecated: 'Use #allMethodSelectors instead' transformWith: '`@receiver allMethods' -> '`@receiver allMethodSelectors'.
	^ self allMethodSelectors
]

Extension { #name : #Character }

{ #category : #'*Deprecated80' }
Character >> basicSqueakToIso [
	
	self deprecated: 'Use #basicPharoToIso instead.' transformWith: '`@receiver basicSqueakToIso' -> '`@receiver basicPharoToIso'.
	^ self basicPharoToIso
]

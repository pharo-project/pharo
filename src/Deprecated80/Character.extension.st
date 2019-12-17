Extension { #name : #Character }

{ #category : #'*Deprecated80' }
Character >> basicSqueakToIso [
	
	self deprecated: 'Use #basicPharoToIso instead.' transformWith: '`@receiver basicSqueakToIso' -> '`@receiver basicPharoToIso'.
	^ self basicPharoToIso
]

{ #category : #'*Deprecated80' }
Character class >> devide [

	self deprecated: 'Use message divide' transformWith: '`@receiver devide' -> '`@receiver divide'.
	
	^self divide
]

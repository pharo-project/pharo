Extension { #name : #ExternalAddress }

{ #category : #'*Fuel-Core' }
ExternalAddress >> fuelAccept: aGeneralMapper [
	aGeneralMapper visitExternalAddress: self
]

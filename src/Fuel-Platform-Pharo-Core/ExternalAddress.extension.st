Extension { #name : #ExternalAddress }

{ #category : #'*Fuel-Platform-Pharo-Core' }
ExternalAddress >> fuelAccept: aGeneralMapper [

	^ aGeneralMapper
		visitSubstitution: self
		by: self class null
		onRecursionDo: [ ^ aGeneralMapper visitFixedObject: self ]
]

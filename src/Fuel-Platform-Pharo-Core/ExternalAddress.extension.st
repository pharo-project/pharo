Extension { #name : #ExternalAddress }

{ #category : #'*Fuel-Platform-Pharo-Core' }
ExternalAddress >> fuelAccept: aGeneralMapper [

	^ aGeneralMapper
		visitSubstitution: self
		by: ExternalAddress null
		onRecursionDo: [ ^ super fuelAccept: aGeneralMapper ]
]

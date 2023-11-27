Extension { #name : 'EphemeronLayout' }

{ #category : '*Fuel-Core' }
EphemeronLayout >> fuelAccept: aGeneralMapper forInstance: anObject [
	aGeneralMapper visitEphemeron: anObject
]

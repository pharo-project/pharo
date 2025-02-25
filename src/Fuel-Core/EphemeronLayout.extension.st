Extension { #name : 'EphemeronLayout' }

{ #category : '*Fuel-Core' }
EphemeronLayout >> fuelAccept: aGeneralMapper forInstance: anObject [
	anObject fuelCheckEphemeronSupport.
	
	aGeneralMapper visitEphemeron: anObject
]

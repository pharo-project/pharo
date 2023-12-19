Extension { #name : 'WeakKeyAssociation' }

{ #category : '*Fuel-Core' }
WeakKeyAssociation >> fuelCheckEphemeronSupport [
	container isDictionary ifFalse: [
		FLNotSerializable signal: 'Unsupported use of ephemeron' ]
]

{ #category : '*Fuel-Core' }
WeakKeyAssociation >> fuelWasMourned [
	^ (self container includesKey: key) not
]

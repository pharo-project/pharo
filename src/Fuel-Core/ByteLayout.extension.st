Extension { #name : 'ByteLayout' }

{ #category : '*Fuel-Core' }
ByteLayout >> fuelAccept: aGeneralMapper forInstance: anObject [
	aGeneralMapper visitBytesObject: anObject
]

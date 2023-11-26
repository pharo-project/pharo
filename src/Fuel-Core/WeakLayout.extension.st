Extension { #name : 'WeakLayout' }

{ #category : '*Fuel-Core' }
WeakLayout >> fuelAccept: aGeneralMapper forInstance: anObject [
	aGeneralMapper visitWeakObject: anObject
]

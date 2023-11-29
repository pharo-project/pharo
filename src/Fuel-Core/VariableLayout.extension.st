Extension { #name : 'VariableLayout' }

{ #category : '*Fuel-Core' }
VariableLayout >> fuelAccept: aGeneralMapper forInstance: anObject [
	aGeneralMapper visitVariableObject: anObject
]

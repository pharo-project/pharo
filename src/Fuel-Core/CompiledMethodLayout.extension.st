Extension { #name : 'CompiledMethodLayout' }

{ #category : '*Fuel-Core' }
CompiledMethodLayout >> fuelAccept: aGeneralMapper forInstance: anObject [
	aGeneralMapper visitBytesObject: anObject
]

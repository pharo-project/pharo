Extension { #name : 'GlobalVariable' }

{ #category : '*Fuel-Core' }
GlobalVariable >> fuelAccept: aGeneralMapper [
	aGeneralMapper visitGlobalVariable: self
]

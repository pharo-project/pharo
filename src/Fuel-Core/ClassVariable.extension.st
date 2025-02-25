Extension { #name : 'ClassVariable' }

{ #category : '*Fuel-Core' }
ClassVariable >> fuelAccept: aGeneralMapper [
	aGeneralMapper visitClassVariable: self
]

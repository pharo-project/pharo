Extension { #name : #Class }

{ #category : #'*Fuel-Core' }
Class >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitClass: self
]

Extension { #name : #Class }

{ #category : #'*Fuel' }
Class >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitClass: self
]

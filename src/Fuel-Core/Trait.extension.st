Extension { #name : #Trait }

{ #category : #'*Fuel-Core' }
Trait >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitTrait: self
]

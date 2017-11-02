Extension { #name : #Trait }

{ #category : #'*Fuel' }
Trait >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitTrait: self
]

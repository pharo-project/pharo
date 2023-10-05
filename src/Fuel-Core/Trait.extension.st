Extension { #name : 'Trait' }

{ #category : '*Fuel-Core' }
Trait >> basicEnvironment [

	^self environment
]

{ #category : '*Fuel-Core' }
Trait >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitTrait: self
]

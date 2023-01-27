Extension { #name : #TraitBehavior }

{ #category : #'*Fuel-Core' }
TraitBehavior >> fuelInitializeTraitComposition [

	self traitComposition traits do: [:each | each addUser: self]
]

{ #category : #'*Fuel-Core' }
TraitBehavior >> fuelRecomputeSlotScopeWithTraits [
	"Nothing to do"
]

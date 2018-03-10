Extension { #name : #MetaclassForTraits }

{ #category : #'*Fuel' }
MetaclassForTraits >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitClassTrait: self

]

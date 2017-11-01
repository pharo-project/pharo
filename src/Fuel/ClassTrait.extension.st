Extension { #name : #ClassTrait }

{ #category : #'*Fuel' }
ClassTrait >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitClassTrait: self

]

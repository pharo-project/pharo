Extension { #name : #ClassTrait }

{ #category : #'*Fuel-Core' }
ClassTrait >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitClassTrait: self

]

Extension { #name : #Metaclass }

{ #category : #'*Fuel-Core' }
Metaclass >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitMetaclass: self

]

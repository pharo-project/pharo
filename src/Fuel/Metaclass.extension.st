Extension { #name : #Metaclass }

{ #category : #'*Fuel' }
Metaclass >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitMetaclass: self

]

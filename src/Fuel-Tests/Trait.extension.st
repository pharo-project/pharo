Extension { #name : #Trait }

{ #category : #'*Fuel-Tests' }
Trait >> renameSilently: aName [
	 [ self rename: aName] fuelValueWithoutNotifications
]

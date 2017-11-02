Extension { #name : #Trait }

{ #category : #'*FuelTests' }
Trait >> renameSilently: aName [
	 [ self rename: aName] fuelValueWithoutNotifications
]

Extension { #name : #Trait }

{ #category : #'*Fuel-Tests-Core' }
Trait >> renameSilently: aName [
	 [ self rename: aName] fuelValueWithoutNotifications
]

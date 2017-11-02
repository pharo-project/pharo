Extension { #name : #Class }

{ #category : #'*FuelTests' }
Class >> renameSilently: aName [
	 [ self rename: aName] fuelValueWithoutNotifications
]

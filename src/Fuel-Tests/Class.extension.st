Extension { #name : #Class }

{ #category : #'*Fuel-Tests' }
Class >> renameSilently: aName [
	 [ self rename: aName] fuelValueWithoutNotifications
]

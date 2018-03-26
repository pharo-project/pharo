Extension { #name : #Class }

{ #category : #'*Fuel-Tests-Core' }
Class >> renameSilently: aName [
	 [ self rename: aName] fuelValueWithoutNotifications
]

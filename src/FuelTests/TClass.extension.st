Extension { #name : #TClass }

{ #category : #'*FuelTests' }
TClass >> renameSilently: aName [
	 [ self rename: aName] fuelValueWithoutNotifications
]

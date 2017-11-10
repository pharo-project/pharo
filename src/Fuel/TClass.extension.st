Extension { #name : #TClass }

{ #category : #'*Fuel' }
TClass >> fuelAccept: aGeneralMapper [
	^self explicitRequirement.
]

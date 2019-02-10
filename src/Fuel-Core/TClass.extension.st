Extension { #name : #TClass }

{ #category : #'*Fuel-Core' }
TClass >> fuelAccept: aGeneralMapper [
	^self explicitRequirement.
]

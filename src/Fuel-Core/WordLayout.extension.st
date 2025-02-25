Extension { #name : 'WordLayout' }

{ #category : '*Fuel-Core' }
WordLayout >> fuelAccept: aGeneralMapper forInstance: anObject [
	aGeneralMapper visitWordsObject: anObject
]

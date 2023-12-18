Extension { #name : 'DoubleWordLayout' }

{ #category : '*Fuel-Core' }
DoubleWordLayout >> fuelAccept: aGeneralMapper forInstance: anObject [
	aGeneralMapper visitWordsObject: anObject
]

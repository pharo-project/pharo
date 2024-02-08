Extension { #name : 'DoubleByteLayout' }

{ #category : '*Fuel-Core' }
DoubleByteLayout >> fuelAccept: aGeneralMapper forInstance: anObject [
	aGeneralMapper visitWordsObject: anObject
]

Extension { #name : 'AbstractLayout' }

{ #category : '*Fuel-Core' }
AbstractLayout >> fuelAccept: aGeneralMapper forInstance: anObject [
	aGeneralMapper visitFixedObject: anObject
]

{ #category : '*Fuel-Core' }
AbstractLayout >> hasSlotScope [
	^ false
]

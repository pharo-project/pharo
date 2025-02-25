Extension { #name : 'SmallInteger' }

{ #category : '*Fuel-Core' }
SmallInteger >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitSmallInteger: self
]

Extension { #name : #Rectangle }

{ #category : #'*Fuel-Core' }
Rectangle >> fuelAccept: aGeneralMapper [
	"Since we can have subclasses of Rectangle, like CharacterBlock that adds new instVars, we cannot use the visitRectangle: for it."
	^ (self class == Rectangle) 
		ifTrue: [ aGeneralMapper visitRectangle: self ]
		ifFalse: [ super fuelAccept: aGeneralMapper   ] 
	

]

{ #category : #'*Fuel-Core' }
Rectangle >> fuelSetOrigin: originPoint corner: cornerPoint [
	origin := originPoint.
	corner := cornerPoint
]

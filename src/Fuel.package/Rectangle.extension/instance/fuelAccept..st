*Fuel
fuelAccept: aGeneralMapper
	"Since we can have subclasses of Rectangle, like CharacterBlock that adds new instVars, we cannot use the visitRectangle: for it."
	^ (self class == Rectangle) 
		ifTrue: [ aGeneralMapper visitRectangle: self ]
		ifFalse: [ super fuelAccept: aGeneralMapper   ] 
	

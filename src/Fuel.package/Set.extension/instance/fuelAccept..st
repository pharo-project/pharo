*Fuel
fuelAccept: aGeneralMapper
	"Since we have subclasses of Set that behave differently, we cannot use the visitSimpleCollection: for all of them."
	^ ((self class == Set) or: [ self class == IdentitySet ] ) 
		ifTrue: [ aGeneralMapper visitSimpleCollection: self ]
		ifFalse: [ super fuelAccept: aGeneralMapper ] 
	
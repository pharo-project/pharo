*Fuel
fuelAccept: aGeneralMapper
	"serialize all instances except for <Smalltalk globals>"
	^ self == Smalltalk globals 
		ifFalse: [ super fuelAccept: aGeneralMapper ]
		ifTrue: [ aGeneralMapper visitHookPrimitive: self ]
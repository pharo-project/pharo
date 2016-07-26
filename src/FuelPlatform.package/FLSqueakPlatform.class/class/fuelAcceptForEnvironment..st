private-extensions-environment
fuelAcceptForEnvironment: aGeneralMapper
	<flExtensionOf: #Environment selector: #fuelAccept:>
	"serialize all instances except for <Smalltalk globals>"
	^ self == Smalltalk globals 
		ifFalse: [ super fuelAccept: aGeneralMapper ]
		ifTrue: [ aGeneralMapper visitHookPrimitive: self ]
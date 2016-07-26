private-extensions-environment
materializeFromForEnvironment: aDecoder
	<flExtensionOf: #Environment selector: #materializeFrom:>
	<flExtensionClassSide>
	"Answer my well-known instance"

	^ (Smalltalk at: #Environment) default
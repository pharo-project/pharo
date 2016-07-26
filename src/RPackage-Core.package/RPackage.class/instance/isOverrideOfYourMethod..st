system compatibility
isOverrideOfYourMethod: aMethodReference
	"Answers true if the argument overrides a method in this package"
	^ (self isYourClassExtension: aMethodReference category) not and:
		[(self changeRecordForOverriddenMethod: aMethodReference) notNil]
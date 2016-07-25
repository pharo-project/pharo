env building
buildPrimitiveEnvArgument
	"Builds the whole env argument as expected by posix_spawn(), taking into account the variables
	defined by the API #environmentAt:put:"
	| envPointer |
	"By default, if none variable was set, we add all the ones existing in parent"
	envVariables isEmpty ifTrue: [ self addAllEnvVariablesFromParentWithoutOverride ].
	"The +1 is because we will have a NULL at the end"
	envPointer := ExternalAddress allocate: (envVariables keys size + 1) * self systemAccessor sizeOfPointer.
	self registerPointer: envPointer.
	self collectEnvVariablesPointersInto: envPointer.
	^ envPointer
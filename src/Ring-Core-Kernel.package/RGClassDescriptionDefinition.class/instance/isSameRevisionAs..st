testing
isSameRevisionAs: aRGClassDescriptionDefinition
	"This method look for equality of the properties of the receiver"
	"Instances variables are compared at the level of names but without any sorting"

	^(super isSameRevisionAs: aRGClassDescriptionDefinition)
		and:[ self instVarNames sort = aRGClassDescriptionDefinition instVarNames sort ]
testing
isSameRevisionAs: aRGMetatraitDefinition
	"This method look for equality of the properties of the receiver"

	^(super isSameRevisionAs: aRGMetatraitDefinition)
		and: [ self traitCompositionSource = aRGMetatraitDefinition traitCompositionSource ]
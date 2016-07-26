testing
isSameRevisionAs: aRGMetaclassDefinition
	"This method look for equality of the properties of the receiver"

	^(super isSameRevisionAs: aRGMetaclassDefinition)
		and:[ self traitCompositionSource = aRGMetaclassDefinition traitCompositionSource ]
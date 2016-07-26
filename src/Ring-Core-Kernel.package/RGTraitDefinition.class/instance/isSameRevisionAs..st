testing
isSameRevisionAs: aRGTraitDefinition
	"This method look for equality of the properties of the receiver"

	^(super isSameRevisionAs: aRGTraitDefinition)
		and: [ self category = aRGTraitDefinition category 
		and: [ self traitCompositionSource = aRGTraitDefinition traitCompositionSource 
		and: [ ((self hasComment and: [ self comment isSameRevisionAs: aRGTraitDefinition comment ]) or:[ self hasComment not ])
		and: [ (self theMetaClass isSameRevisionAs: aRGTraitDefinition theMetaClass) ] ] ] ]
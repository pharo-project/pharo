testing
isSameRevisionAs: aRGClassDefinition
	"This method look for equality of the properties of the receiver"

	^(super isSameRevisionAs: aRGClassDefinition)
		and:[ self superclassName == aRGClassDefinition superclassName
		and:[ self category = aRGClassDefinition category 
		and:[ self classVarNames sorted = aRGClassDefinition classVarNames sorted 
		and:[ self sharedPoolNames sorted = aRGClassDefinition sharedPoolNames sorted  
		and:[ self traitCompositionSource = aRGClassDefinition traitCompositionSource 
		and:[ ((self hasComment and:[ self comment isSameRevisionAs: aRGClassDefinition comment ]) or:[ self hasComment not ])
		and:[ (self theMetaClass isSameRevisionAs: aRGClassDefinition theMetaClass) ] ] ] ] ] ] ]
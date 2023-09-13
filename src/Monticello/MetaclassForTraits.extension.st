Extension { #name : #MetaclassForTraits }

{ #category : #'*Monticello' }
MetaclassForTraits >> asMCDefinition [

	^ (MCClassTraitDefinition baseTraitName: self instanceSide name)
		  classTraitComposition: self traitCompositionString;
		  packageName: self package name;
		  tagName: self packageTag name;
		  yourself
]

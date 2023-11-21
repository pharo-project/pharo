Extension { #name : 'RGTraitStrategy' }

{ #category : '*Ring-Monticello' }
RGTraitStrategy >> asMCDefinition [
	"We are missing the package tag here. We should add it"
	^ (MCTraitDefinition named: self owner name)
		  traitComposition: self owner traitCompositionString;
		  packageName: self package name;
		  classTraitComposition: self classSide traitCompositionString;
		  comment: self comment content;
		  yourself
]

error-handling
errorIfNotAddable: aTraitTransformation
	(self includesTrait: aTraitTransformation trait) ifTrue: [
		^TraitCompositionException
			signal: 'Trait ' , aTraitTransformation trait asString, ' already in composition']
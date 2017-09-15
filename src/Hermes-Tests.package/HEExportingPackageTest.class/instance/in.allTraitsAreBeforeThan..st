as yet unclassified
in: aSeq allTraitsAreBeforeThan: aTrait
	| myIndex |
	myIndex := aSeq indexOf: aTrait.
	^ (aTrait traitComposition allDependenciesIn: aSeq) allSatisfy: [ :aUsedTrait | (aSeq indexOf: aUsedTrait) < myIndex ]
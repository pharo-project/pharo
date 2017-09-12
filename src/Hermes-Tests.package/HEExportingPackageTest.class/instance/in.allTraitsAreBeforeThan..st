as yet unclassified
in: aSeq allTraitsAreBeforeThan: aTrait
	| myIndex realTrait |
	myIndex := aSeq indexOf: aTrait.
	realTrait := Smalltalk at: aTrait traitName.
	
	^ realTrait traitComposition traits allSatisfy: [ : aUsedTrait | 
		(aSeq detectIndex: [ :e | e traitName = aUsedTrait name ] ifNone: [ -1 ]) < myIndex ]
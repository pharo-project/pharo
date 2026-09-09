"
MCClassTraitParser identifies classTrait.

MCClassTraitDefinition are not generated anymore by Pharo. This is just here for backward compatibility.
"
Class {
	#name : 'MCClassTraitParser',
	#superclass : 'MCDoItParser',
	#category : 'Monticello-BackwardCompatibility',
	#package : 'Monticello-BackwardCompatibility'
}

{ #category : 'factory identification hook' }
MCClassTraitParser class >> pattern [
	^ '*classTrait*uses:*'
]

{ #category : 'actions' }
MCClassTraitParser >> addDefinitionsTo: aCollection [

	| tokens definition traitCompositionString |
	tokens := OCParser parseLiterals: source.
	traitCompositionString := (source readStream
		                           match: 'uses:';
		                           upToEnd) trimBoth.
	definition := (MCClassTraitDefinition baseTraitName: (tokens at: 1))
		              classTraitComposition: traitCompositionString;
		              yourself.
	aCollection add: definition
]

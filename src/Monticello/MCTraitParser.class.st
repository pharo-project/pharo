"
A MCTraitParser extracts an MCTraitDefinition from the source.
"
Class {
	#name : #MCTraitParser,
	#superclass : #MCDoItParser,
	#category : #'Monticello-Chunk Format'
}

{ #category : #'factory identification hook' }
MCTraitParser class >> pattern [
	^ 'Trait named:*'
]

{ #category : #actions }
MCTraitParser >> addDefinitionsTo: aCollection [
	| tokens  definition traitCompositionString |
	tokens := source parseLiterals.
	traitCompositionString := (source readStream
		match: 'uses:';
		upToAll: 'category:') trimBoth.
	definition := MCTraitDefinition
		name: (tokens at: 3) 
		traitComposition: traitCompositionString
		category:  tokens last
		comment:  ''  
		commentStamp:   ''.
	aCollection add: definition.
]

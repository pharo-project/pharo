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
	| node compositionIndex categoryIndex slotsIndex traitName traitCompositionString slotsArray argumentNode categoryString definition |
	 
	node := RBParser parseExpression: source.
	
	node selector caseOf: { 
		[#named:] -> [ 
			compositionIndex := nil. slotsIndex := nil. categoryIndex := nil ].
		[#named:uses:] -> [ 
			compositionIndex := 2. slotsIndex := nil. categoryIndex := nil ].
		[#named:uses:category:] -> [ 
			compositionIndex := 2. slotsIndex := nil. categoryIndex := 3 ].
		[#named:uses:package:] -> [ 
			compositionIndex := 2. slotsIndex := nil. categoryIndex := 3 ].
		[#named:uses:slots:category:] -> [ 
			compositionIndex := 2. slotsIndex := 3. categoryIndex := 4 ].
		[#named:uses:slots:package:] -> [ 
			compositionIndex := 2. slotsIndex := 3. categoryIndex := 4 ]
	} otherwise: [ self error: 'Unknown trait building selector' ].

	traitName := node arguments first value.

	traitCompositionString := compositionIndex 
		ifNotNil: [ 
			argumentNode := node arguments at: compositionIndex.
			argumentNode source copyFrom: argumentNode sourceInterval first to: argumentNode sourceInterval last ]
		ifNil: [ '' ].

	slotsArray := slotsIndex 
		ifNotNil: [ 
			argumentNode := node arguments at: slotsIndex.
			argumentNode statements collect: [ :each |
				each source copyFrom: each sourceInterval first to: each sourceInterval last ] ]
		ifNil: [ Array new ].

	categoryString := categoryIndex 
		ifNotNil: [ 
			argumentNode := node arguments at: categoryIndex.
			argumentNode source copyFrom: argumentNode sourceInterval first to: argumentNode sourceInterval last ]
		ifNil: [ 'Unclassified' ].

	definition := MCTraitDefinition
		name: traitName
		traitComposition: traitCompositionString
		category:  categoryString
		instVarNames:  slotsArray
		comment:  ''  
		commentStamp: ''.
		
	aCollection add: definition.
]

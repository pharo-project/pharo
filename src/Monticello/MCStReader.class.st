"
I read Category/Classes/Methodes/.... definitions from Monticello source.st file format.

See also MCStWriter.

Example:

|source|
source := String streamContents: [:aStream| |writer|
    writer := MCStWriter on: aStream.
    writer writeDefinitions: {True asClassDefinition. False asClassDefinition}.
].

(MCStReader on: source readStream) definitions explore.
"
Class {
	#name : #MCStReader,
	#superclass : #MCSnapshotReader,
	#category : #'Monticello-Storing'
}

{ #category : #accessing }
MCStReader class >> extension [
	^ 'st'
]

{ #category : #adding }
MCStReader >> addDefinitionsFromDoit: aString [
	(MCDoItParser forDoit: aString) ifNotNil:
		[:parser |
		parser addDefinitionsTo: definitions]
]

{ #category : #reading }
MCStReader >> categoryFromDoIt: aString [
	| tokens  |
	tokens := aString parseLiterals.
	tokens size = 3 ifFalse: [self error: 'Unrecognized category definition'].
	^ tokens at: 3
]

{ #category : #reading }
MCStReader >> classDefinitionFrom: aRingClass [
	| tokens traitCompositionString lastIndex classTraitCompositionString |
	tokens := aRingClass definitionSource parseLiterals.
	traitCompositionString := (aRingClass definitionSource readStream
		match: 'uses:';
		upToAll: 'instanceVariableNames:') trimBoth.
	classTraitCompositionString := (aRingClass classSide definitionSource asString readStream
		match: 'uses:';
		upToAll: 'instanceVariableNames:') trimBoth.
	traitCompositionString isEmpty ifTrue: [traitCompositionString := '{}'].
	classTraitCompositionString isEmpty ifTrue: [classTraitCompositionString := '{}'].
	lastIndex := tokens size.
	^ MCClassDefinition
		name: (tokens at: 3)
		superclassName: (tokens at: 1)
		traitComposition: traitCompositionString
		classTraitComposition: classTraitCompositionString
		category: (tokens at: lastIndex)
		instVarNames: ((tokens at: lastIndex - 6) findTokens: ' ')
		classVarNames: ((tokens at: lastIndex - 4) findTokens: ' ')
		poolDictionaryNames: ((tokens at: lastIndex - 2) findTokens: ' ')
		classInstVarNames: (self classInstVarNamesFor: aRingClass)
		type: (self typeOfSubclass: (tokens at: 2))
		comment: (self commentFor: aRingClass)
		commentStamp: (self commentStampFor: aRingClass)
]

{ #category : #reading }
MCStReader >> classInstVarNamesFor: aRingClass [
	| tokens |
	
	self flag: #traits.
	aRingClass classSide hasDefinitionSource ifFalse: [^ #()].
	tokens := aRingClass classSide definitionSource parseLiterals.
	"tokens size = 4 ifFalse: [self error: 'Unrecognized metaclass definition']."
	^ tokens last findTokens: ' '
]

{ #category : #reading }
MCStReader >> commentFor: aRingClass [

	^ aRingClass hasComment
		ifTrue: [ aRingClass comment content asString ]
		ifFalse: [ '' ]
]

{ #category : #reading }
MCStReader >> commentStampFor: aRingClass [
	^ aRingClass hasComment
		ifTrue: [ aRingClass comment stamp ]
		ifFalse: [ nil ]
]

{ #category : #evaluating }
MCStReader >> loadDefinitions [
	| filePackage |
	filePackage := RingChunkImporter fromStream: self readStream.
	definitions := OrderedCollection new.
	filePackage classes
		do: [ :ringClass |
			ringClass hasDefinitionSource
				ifTrue: [definitions add:
						(self classDefinitionFrom: ringClass)].
			definitions addAll: (self methodDefinitionsFor: ringClass).
			definitions addAll: (self methodDefinitionsFor: ringClass classSide) ].
	filePackage doIts 
		do: [ :ea | self addDefinitionsFromDoit: ea contents ].
	
]

{ #category : #reading }
MCStReader >> methodDefinitionsFor: aRingClass [

	^ aRingClass methods collect: 
		[ :each | each asMCMethodDefinition ]
]

{ #category : #evaluating }
MCStReader >> readStream [
	^ ('!!

', stream contents) readStream
]

{ #category : #reading }
MCStReader >> systemOrganizationFromRecords: changeRecords [
	| categories |
	categories := changeRecords
					select: [:ea | 'SystemOrganization*' match: ea string]
					thenCollect: [:ea | (self categoryFromDoIt: ea string)].
	^ categories isEmpty ifFalse: [MCOrganizationDefinition categories: categories asArray]
]

{ #category : #reading }
MCStReader >> typeOfSubclass: aSymbol [
	#(
		(subclass: normal)
		(named: normal)
		(variableSubclass: variable)
		(variableByteSubclass: bytes)
		(variableWordSubclass: words)
		(weakSubclass: weak)
		) do: [:ea | ea first = aSymbol ifTrue: [^ ea second]].
	self error: 'Unrecognized class definition'
]

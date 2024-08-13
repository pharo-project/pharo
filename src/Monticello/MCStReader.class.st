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
	#name : 'MCStReader',
	#superclass : 'MCReader',
	#instVars : [
		'definitions'
	],
	#category : 'Monticello-Storing',
	#package : 'Monticello',
	#tag : 'Storing'
}

{ #category : 'accessing' }
MCStReader class >> extension [
	^ 'st'
]

{ #category : 'adding' }
MCStReader >> addDefinitionsFromDoit: aString [
	(MCDoItParser forDoit: aString) ifNotNil:
		[:parser |
		parser addDefinitionsTo: definitions]
]

{ #category : 'reading' }
MCStReader >> classDefinitionFrom: aRingClass [

	| tokens traitCompositionString lastIndex classTraitCompositionString converter |
	tokens := aRingClass definitionSource parseLiterals.
	traitCompositionString := (aRingClass definitionSource readStream
		                           match: 'uses:';
		                           upToAll: 'instanceVariableNames:') trimBoth.
	classTraitCompositionString := (aRingClass classSide definitionSource asString readStream
		                                match: 'uses:';
		                                upToAll: 'instanceVariableNames:') trimBoth.
	traitCompositionString ifEmpty: [ traitCompositionString := '{}' ].
	classTraitCompositionString ifEmpty: [ classTraitCompositionString := '{}' ].
	lastIndex := tokens size.
	converter := CategoryConverter category: (tokens at: lastIndex).
	^ (MCClassDefinition named: (tokens at: 3))
		  superclassName: (tokens at: 1);
		  traitComposition: traitCompositionString;
		  classTraitComposition: classTraitCompositionString;
		  packageName: converter packageName;
		  tagName: converter tagName;
		  instVarNames: ((tokens at: lastIndex - 6) findTokens: ' ');
		  classVarNames: ((tokens at: lastIndex - 4) findTokens: ' ');
		  poolDictionaryNames: ((tokens at: lastIndex - 2) findTokens: ' ');
		  classInstVarNames: (self classInstVarNamesFor: aRingClass);
		  type: (self typeOfSubclass: (tokens at: 2));
		  comment: (self commentFor: aRingClass);
		  commentStamp: (self commentStampFor: aRingClass);
		  yourself
]

{ #category : 'reading' }
MCStReader >> classInstVarNamesFor: aRingClass [
	| tokens |
	
	self flag: #traits.
	aRingClass classSide hasDefinitionSource ifFalse: [^ #()].
	tokens := aRingClass classSide definitionSource parseLiterals.
	"tokens size = 4 ifFalse: [self error: 'Unrecognized metaclass definition']."
	^ tokens last findTokens: ' '
]

{ #category : 'reading' }
MCStReader >> commentFor: aRingClass [

	^ aRingClass hasComment
		ifTrue: [ aRingClass comment content asString ]
		ifFalse: [ '' ]
]

{ #category : 'reading' }
MCStReader >> commentStampFor: aRingClass [
	^ aRingClass hasComment
		ifTrue: [ aRingClass comment stamp ]
		ifFalse: [ nil ]
]

{ #category : 'accessing' }
MCStReader >> definitions [
	definitions ifNil: [self loadDefinitions].
	^ definitions
]

{ #category : 'evaluating' }
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

{ #category : 'reading' }
MCStReader >> methodDefinitionsFor: aRingClass [

	^ aRingClass methods collect: [ :each | each asMCMethodDefinition ]
]

{ #category : 'evaluating' }
MCStReader >> readStream [
	^ ('!!

', stream contents) readStream
]

{ #category : 'accessing' }
MCStReader >> snapshot [
	^ MCSnapshot fromDefinitions: self definitions
]

{ #category : 'reading' }
MCStReader >> typeOfSubclass: aSymbol [
	| layoutClass |
	layoutClass := ObjectLayout layoutForSubclassDefiningSymbol: aSymbol.
	"if we get back CompiledMethodLayout, we put ByteLayout as CompiledMethodLayout.
	The actual class creation methods have a hack to create the right format in this case"
	layoutClass = CompiledMethodLayout ifTrue: [ layoutClass := ByteLayout ].
	^layoutClass mcTypeSymbol
]

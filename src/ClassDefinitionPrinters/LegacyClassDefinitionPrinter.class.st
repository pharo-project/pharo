"
This is a printer that generates the ST80 class defintion syntax (that is, including pool defintion and category:).

The reason we still need this is that there are still parts of the system that use this in Monticello and the Refactoring engine.

When the last users have been fixed, we can remove this class
"
Class {
	#name : 'LegacyClassDefinitionPrinter',
	#superclass : 'ClassDefinitionPrinter',
	#category : 'ClassDefinitionPrinters',
	#package : 'ClassDefinitionPrinters'
}

{ #category : 'public API' }
LegacyClassDefinitionPrinter >> classDefinitionString [

	"Answer a String that defines the receiver in the old format using poolDictionaries and category.
	e.g,
	'Object subclass: #Point
		instanceVariableNames: ''x y''
		classVariableNames: ''''
		poolDictionaries: ''''
		category: ''Kernel-BasicObjects'''
	"

	| aStream |
	aStream := (String new: 800) writeStream.
	forClass superclass
		ifNil: [aStream nextPutAll: 'ProtoObject']
		ifNotNil: [aStream nextPutAll: forClass superclass name].
	aStream nextPutAll: forClass kindOfSubclass;
			store: forClass name.
	(forClass hasTraitComposition) ifTrue: [
		aStream cr; tab; nextPutAll: 'uses: ';
			nextPutAll: forClass traitCompositionString].
	aStream cr; tab; nextPutAll: 'instanceVariableNames: ';
			store: forClass instanceVariablesString.
	aStream cr; tab; nextPutAll: 'classVariableNames: ';
			store: forClass classVariablesString.
	aStream cr; tab; nextPutAll: 'poolDictionaries: ';
			store: forClass sharedPoolsString.
	aStream cr; tab; nextPutAll: 'category: ';
			store: forClass category asString.

	forClass superclass ifNil: [
		aStream nextPutAll: '.'; cr.
		aStream nextPutAll: forClass name.
		aStream space; nextPutAll: 'superclass: nil'. ].

	^ aStream contents
]

{ #category : 'public API' }
LegacyClassDefinitionPrinter >> metaclassDefinitionString [

	^ String streamContents: [:stream |
		stream print: forClass.
		stream
			crtab;
			nextPutAll: 'instanceVariableNames: ';
			store: forClass instanceVariablesString ]
]

{ #category : 'public API' }
LegacyClassDefinitionPrinter >> traitDefinitionString [

	^ String streamContents: [ :s |
		s
			nextPutAll: 'Trait named: ';
			nextPutAll:	forClass name printString;
			cr; tab;
			nextPutAll: ' uses: ';
			nextPutAll: forClass traitComposition traitCompositionExpression;
			cr;
			tab;
			nextPutAll: ' package: ';
			nextPutAll: forClass category asString printString
	]
]

{ #category : 'public API' }
LegacyClassDefinitionPrinter >> traitedMetaclassDefinitionString [

	^ self metaclassDefinitionString
]

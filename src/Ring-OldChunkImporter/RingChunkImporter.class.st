"
I'm an object holding the result of loading a file containing Pharo code definitions in chunk format.
I create ring definitions for the elements inside the chunk stream.
Heavily inspired from FilePackage.

see example method class side 
"
Class {
	#name : 'RingChunkImporter',
	#superclass : 'Object',
	#instVars : [
		'package',
		'doIts'
	],
	#category : 'Ring-OldChunkImporter-Base',
	#package : 'Ring-OldChunkImporter',
	#tag : 'Base'
}

{ #category : 'examples' }
RingChunkImporter class >> example [

	| internalStream |
	internalStream := (String new: 1000) writeStream.
	((self packageOrganizer packageNamed: 'Ring-OldChunkImporter') tagNamed: #Base) fileOutOn: internalStream.
	(self fromStream: internalStream contents readStream) inspect
]

{ #category : 'instance creation' }
RingChunkImporter class >> fromFileNamed: fileName [

	^ self new fromFileNamed: fileName; yourself
]

{ #category : 'instance creation' }
RingChunkImporter class >> fromStream: aStream [

	^ self new fileInFrom: aStream; yourself
]

{ #category : 'private' }
RingChunkImporter >> classDefinition: aString with: chgRec [

	| tokens theClass converter |
	tokens := self scannedSource: aString.
	theClass := self classNamed: tokens third.
	theClass
		superclassName: tokens first;
		definitionSource: aString.

	converter := CategoryConverter category: tokens last.

	theClass
		package: (RGPackageDefinition named: converter packageName);
		packageTag: converter tagName.

	tokens size = 11 ifTrue: [
		theClass
			instanceVariables: (tokens fifth findTokens: ' ');
			classVariables: (tokens seventh findTokens: ' ');
			sharedPools: (tokens ninth findTokens: ' ') ].

	tokens size = 13 ifTrue: [
		theClass
			traitCompositionSource: tokens fifth;
			instanceVariables: (tokens seventh findTokens: ' ');
			classVariables: (tokens ninth findTokens: ' ');
			sharedPools: ((tokens at: 11) findTokens: ' ') ]
]

{ #category : 'private' }
RingChunkImporter >> classNamed: className [

	(package includesClassNamed: className)
		ifTrue:[ ^ package classNamed: className ].
	^ self createClassNamed: className
]

{ #category : 'accessing' }
RingChunkImporter >> classes [
	^ package classes values
]

{ #category : 'private' }
RingChunkImporter >> createClassNamed: className [
	| rgClass |
	rgClass := RGClassDefinition named: className.
	rgClass withMetaclass.
	package addClass: rgClass.
	^ rgClass
]

{ #category : 'private' }
RingChunkImporter >> createTraitNamed: traitName [

	| trait |
	trait := RGTraitDefinition named: traitName.
	trait withMetaclass.
	package addTrait: trait.
	^ trait
]

{ #category : 'accessing' }
RingChunkImporter >> doIts [
	^ doIts
]

{ #category : 'reading' }
RingChunkImporter >> fileInFrom: aStream [

	| changes |
	changes := (CodeImporter readStream: aStream) parseChunks.
	changes do: [ :change | change accept: self ]
]

{ #category : 'reading' }
RingChunkImporter >> fromFileNamed: aName [

	package := RGPackageDefinition named: aName.
	self fileInFrom: aName asFileReference readStream
]

{ #category : 'initialization' }
RingChunkImporter >> initialize [
	super initialize.
	package := RGPackageDefinition named: 'ChangeSet'.
	doIts := Set new
]

{ #category : 'private' }
RingChunkImporter >> metaClassDefinition: string with: chgRec [
	| tokens theClass |

	tokens := self scannedSource: string.

	theClass := self classNamed: tokens first.
	theClass classSide definitionSource: string;
			  instanceVariables: (tokens fourth findTokens: ' ')
]

{ #category : 'private' }
RingChunkImporter >> metaTraitDefinition: string with: chgRec [
	| tokens trait |

	tokens := self scannedSource: string.

	trait := self classNamed: tokens first.
	trait isTrait
		ifFalse:[ trait:= self createTraitNamed: tokens first ].

	trait classSide definitionSource: string
]

{ #category : 'private' }
RingChunkImporter >> msgClassComment: string with: chgRec [

	| tokens theClass |
	tokens := self scannedSource: string.

	(tokens size >= 3
		and:[ tokens last isString ])
	ifTrue:[
		theClass := self classNamed: tokens first.
		((tokens at: (tokens size - 1)) == #class
		ifTrue: [ theClass classSide ]
		ifFalse:[ theClass ]) comment: tokens last asString ]
]

{ #category : 'accessing' }
RingChunkImporter >> packages [

	^ { package }
]

{ #category : 'private' }
RingChunkImporter >> removedMethod: string with: aChunk [

	| orClass tokens |
	tokens := self scannedSource: string.
	tokens size >= 3
		ifTrue:[ orClass := self classNamed: tokens first.
		^ (tokens second == #class
			ifTrue: [ orClass classSide ]
			ifFalse:[ orClass ]) removeSelector: tokens last ].
	doIts add: aChunk
]

{ #category : 'private' }
RingChunkImporter >> scannedSource: aString [

	^ Smalltalk compiler parseLiterals: aString
]

{ #category : 'private' }
RingChunkImporter >> traitDefinition: aString with: chgRec [

	| tokens trait converter |
	tokens := self scannedSource: aString.
	trait := self classNamed: tokens third.
	trait isTrait ifFalse: [ trait := self createTraitNamed: tokens third ].
	trait
		superclassName: tokens first;
		definitionSource: aString.

	converter := CategoryConverter category: tokens last.

	trait
		package: (RGPackageDefinition named: converter packageName);
		packageTag: converter tagName
]

{ #category : 'visitor' }
RingChunkImporter >> visitClassCommentChunk: aChunk [

	(self classNamed: aChunk behaviorName)
		comment: aChunk contents;
		stamp: aChunk stamp
]

{ #category : 'visitor' }
RingChunkImporter >> visitClassOrganizationChunk: aChunk [

	"Do nothing with class organizations for the moment"
]

{ #category : 'visitor' }
RingChunkImporter >> visitDoItChunk: aChunk [

	| contents |
	contents := aChunk contents.
	('*ubclass:*instanceVariableNames:*'
		match: contents) ifTrue:[^self classDefinition: contents with: aChunk].
	('* class*instanceVariableNames:*'
		match: contents) ifTrue:[^self metaClassDefinition: contents with: aChunk].
	('Trait*named:*uses:*category:*'
		match: contents) ifTrue:[^self traitDefinition: contents with: aChunk].
	('* classTrait*uses:*'
		match: contents) ifTrue:[^self metaTraitDefinition: contents with: aChunk].
	('* removeSelector:*'
		match: contents) ifTrue:[^self removedMethod: contents with: aChunk].
	('* comment:*'
		match: contents) ifTrue:[^self msgClassComment: contents with: aChunk].
	('* initialize'
		match: contents) ifTrue:[^self]. "Initialization is done based on class>>initialize"
	doIts add: aChunk
]

{ #category : 'visitor' }
RingChunkImporter >> visitMethodChunk: aChunk [

	| theClass theMethod |
	theClass := self classNamed: aChunk behaviorName.
	theMethod := (RGMethodDefinition class: theClass selector: aChunk methodSelector)
		             protocol: aChunk protocol;
		             sourceCode: aChunk contents;
		             stamp: aChunk stamp.
	aChunk isMeta
		ifTrue: [
			theMethod isMeta: true.
			theClass classSide addMethod: theMethod ]
		ifFalse: [ theClass addMethod: theMethod ]
]

{ #category : 'visitor' }
RingChunkImporter >> visitStyleChunk: aChunk [

	"Do nothing with styles"
]

"
I write Category/Classes/Methods/.... definitions into Monticello source.st file format found in .mcz packages.    

See also MCStReader

Example:

String streamContents: [:aStream| |writer|
    writer := MCStWriter on: aStream.
    writer writeDefinitions: {True asClassDefinition. False asClassDefinition}.
]
"
Class {
	#name : 'MCStWriter',
	#superclass : 'MCWriter',
	#instVars : [
		'initializers',
		'writeInitializers'
	],
	#category : 'Monticello-Storing',
	#package : 'Monticello',
	#tag : 'Storing'
}

{ #category : 'accessing' }
MCStWriter class >> readerClass [
	^ MCStReader
]

{ #category : 'writing' }
MCStWriter >> basicChunkContents: aBlock [
	stream nextPut: (String streamContents: aBlock)
]

{ #category : 'writing' }
MCStWriter >> chunkContents: aBlock [
	stream
		cr;
		nextPut: (String streamContents: aBlock);
		cr
]

{ #category : 'initialization' }
MCStWriter >> stream: aStream [

	stream := ChunkWriteStream on: aStream
]

{ #category : 'visiting' }
MCStWriter >> visitClassDefinition: definition [
	self writeClassDefinition: definition.
	definition hasClassInstanceVariables ifTrue: [self writeMetaclassDefinition: definition].
	definition hasComment ifTrue: [self writeClassComment: definition].
]

{ #category : 'visiting' }
MCStWriter >> visitClassTraitDefinition: definition [
	self chunkContents: [:s | s
		nextPutAll: definition baseTrait;
		nextPutAll: ' classTrait';
		cr; tab;
		nextPutAll: 'uses: ';
		nextPutAll: (definition classTraitComposition ifNil: ['{}'])]

]

{ #category : 'visiting' }
MCStWriter >> visitMetaclassDefinition: definition [
	self writeMetaclassDefinition: definition
]

{ #category : 'visiting' }
MCStWriter >> visitMethodDefinition: definition [
	self writeMethodPreamble: definition.
	self writeMethodSource: definition.
	self writeMethodPostscript.
	self writeInitializers ifTrue: [ 
		self writeMethodInitializer: definition ].
]

{ #category : 'visiting' }
MCStWriter >> visitOrganizationDefinition: definition [

	self writePackage: definition packageName tags: (definition tagNames ifNil: [ #(  ) ])
]

{ #category : 'visiting' }
MCStWriter >> visitScriptDefinition: definition [
	self writeScriptDefinition: definition

]

{ #category : 'visiting' }
MCStWriter >> visitTraitDefinition: definition [
	self writeClassDefinition: definition.
	definition hasComment ifTrue: [self writeClassComment: definition].
]

{ #category : 'writing' }
MCStWriter >> writeClassComment: definition [

	self chunkContents: [ :str |
		stream bang.
		str nextPutAll: definition className;
			nextPutAll: ' commentStamp: ';
			store: definition commentStamp;
			nextPutAll: ' prior: 0' ].
	self basicChunkContents: [ :str | str nextPutAll: definition comment ].
	stream cr.
]

{ #category : 'writing' }
MCStWriter >> writeClassDefinition: definition [
	self chunkContents: [:s | definition printDefinitionOn: s].
]

{ #category : 'writing' }
MCStWriter >> writeDefinitions: aCollection [
	
	"the correct initialization order is unknown if some classes are missing in the image"
	
	| presentInitializers notPresentInitializers orderedClasses |
	
	initializers := Set new.

	(MCDependencySorter sortItems: aCollection)
		do: [:ea | ea accept: self]
		displayingProgress: 'Writing definitions...'.
		
	presentInitializers := initializers select: [:each | Smalltalk hasClassNamed: each key ].
	notPresentInitializers := initializers reject: [:each | Smalltalk hasClassNamed: each key ].
	
	orderedClasses := (Class superclassOrder: (presentInitializers collect: [:each | Smalltalk classOrTraitNamed: each key]))
		collect: [:each | each name ].
				
	orderedClasses do: [:className |
		self chunkContents: [ :str | str nextPutAll: (presentInitializers detect: [:each | each key = className]) value contents] ].
	
	(notPresentInitializers asSortedCollection: [:a :b | a key <= b key]) do: [:association |
		self chunkContents: [ :str | str  nextPutAll: association value contents ] ].
]

{ #category : 'accessing' }
MCStWriter >> writeInitializers [

	"answer if class initializers should be written"

	^ writeInitializers ifNil: [ writeInitializers := true ]
]

{ #category : 'accessing' }
MCStWriter >> writeInitializers: aBoolean [

	writeInitializers := aBoolean
]

{ #category : 'writing' }
MCStWriter >> writeMetaclassDefinition: definition [
	self chunkContents: [:str |
		str	 nextPutAll: definition className;
			nextPutAll: ' class';
			cr; tab.
			definition hasClassTraitComposition ifTrue: [
				str	 nextPutAll: 'uses: ';
					nextPutAll: definition classTraitCompositionString;
					cr; tab].
			str	 nextPutAll: 'instanceVariableNames: ''';
				nextPutAll: definition classInstanceVariablesString;
				nextPut: $']
]

{ #category : 'writing' }
MCStWriter >> writeMethodInitializer: aMethodDefinition [
	
	aMethodDefinition isInitializer ifTrue: [ 
		self chunkContents: [ :str |
			str nextPutAll: aMethodDefinition className, ' initialize'.
			initializers add: (aMethodDefinition className -> str) ]
		].
]

{ #category : 'writing' }
MCStWriter >> writeMethodPostscript [
	self basicChunkContents: [:str | str space ].
	stream cr.
]

{ #category : 'writing' }
MCStWriter >> writeMethodPreamble: definition [
	self chunkContents: [:str |
		stream bang.
		str nextPutAll: definition className asString.
		definition classIsMeta ifTrue:
			[ str nextPutAll: ' class' ].
		str
			nextPutAll: ' methodsFor: ';
			nextPutAll: definition protocol asString printString;
			nextPutAll: ' stamp: ';
			nextPutAll: definition timeStamp asString printString
		]
]

{ #category : 'writing' }
MCStWriter >> writeMethodSource: definition [
	self basicChunkContents: [:str | str nextPutAll: definition source ].
]

{ #category : 'writing' }
MCStWriter >> writePackage: packageName tags: aCollection [

	stream
		nextChunkPut: ('self packageOrganizer ensurePackage: {1} withTags: {2}' format: {
						 packageName storeString.
						 aCollection storeString });
		cr
]

{ #category : 'writing' }
MCStWriter >> writeScriptDefinition: definition [
	self chunkContents: [:str | 
		str nextPutAll: (
			'(PackageInfo named: {1}) {2}: {3}'
			format: {
				"{1}" definition packageName printString.
				"{2}" definition scriptSelector. 
				"{3}" definition script printString
			}) ]
]

{ #category : 'writing' }
MCStWriter >> writeSnapshot: aSnapshot [
	self writeDefinitions: aSnapshot definitions
]

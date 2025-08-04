Class {
	#name : 'EpFileOutModificationsTest',
	#superclass : 'EpEnabledIntegrationTest',
	#instVars : [
		'chunkExporter',
		'chunkImporter'
	],
	#category : 'EpiceaBrowsers-Tests-Integration',
	#package : 'EpiceaBrowsers-Tests',
	#tag : 'Integration'
}

{ #category : 'accessing' }
EpFileOutModificationsTest >> chunkExporter [

	^ chunkExporter
]

{ #category : 'accessing' }
EpFileOutModificationsTest >> chunkExporter: anObject [

	chunkExporter := anObject
]

{ #category : 'accessing' }
EpFileOutModificationsTest >> chunkImporter [

	^ chunkImporter
]

{ #category : 'accessing' }
EpFileOutModificationsTest >> chunkImporter: anObject [

	chunkImporter := anObject
]

{ #category : 'running' }
EpFileOutModificationsTest >> setUp [

	super setUp.
	self chunkExporter: EpChunkExporter new.
	self chunkImporter: CodeImporter.
]

{ #category : 'running' }
EpFileOutModificationsTest >> tearDown [

	chunkExporter outputFileReference ensureDelete.
	super tearDown.
]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutMultipleClassAddition [

	| aClass1 aClass2 outputFileReference chunks literalNode1 literalNode2 |

	aClass1 := classFactory newClass.
	aClass2 := classFactory newClass.

	chunkExporter fileOut: (self allLogEntriesWith: EpClassAddition).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 3.
	self assert: ((chunks last: 2) allSatisfy: #isDoItDeclaration).

	literalNode1 := (OCParser parseExpression: (chunks at: chunks size - 1) contents) allChildren select: [ :each | each isLiteralNode ].
	literalNode2 := (OCParser parseExpression: chunks last contents) allChildren select: [ :each | each isLiteralNode ].

	self assert: (literalNode1 anySatisfy: [ :node | node value = aClass1 name ]).
	self assert: (literalNode2 anySatisfy: [ :node | node value = aClass2 name ]).

]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutMultipleMethodAddition [

	| aClass outputFileReference chunks methodNode1 methodNode2 |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^ 42'.
	aClass compile: 'thirtyTwo ^ 32'.

	chunkExporter fileOut: (self allLogEntriesWith: EpMethodAddition).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 3.
	self assert: ((chunks last: 2) allSatisfy: #isMethodDeclaration).

	methodNode1 := OCParser parseMethod: (chunks at: chunks size - 1) contents.
	methodNode2 := OCParser parseMethod: chunks last contents.

	self assert: methodNode1 selector equals: #fortyTwo.
	self 
		assert: methodNode1 body value 
		equals: (OCSequenceNode statements:{ (OCReturnNode value: (OCLiteralValueNode value: 42)) }).
		
	self assert: methodNode2 selector equals: #thirtyTwo.
	self 
		assert: methodNode2 body value 
		equals: (OCSequenceNode statements:{ (OCReturnNode value: (OCLiteralValueNode value: 32)) })
]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutMultipleMethodModification [

	| aClass outputFileReference chunks methodNode |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^ 42'.
	aClass compile: 'fortyTwo ^ 0.42'.
	aClass compile: 'fortyTwo ^ 0.042'.	

	chunkExporter fileOut: (self allLogEntriesWith: EpMethodModification).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 3.
	self assert: chunks last isMethodDeclaration.

	methodNode := OCParser parseMethod: chunks last contents.

	self assert: methodNode selector equals: #fortyTwo.
	self 
		assert: methodNode body value 
		equals: (OCSequenceNode statements:{ (OCReturnNode value: (OCLiteralValueNode value: 0.042)) }).

]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutMultipleMethodRemoval [

	| aClass outputFileReference chunks methodNode1 methodNode2 |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^ 42'.
	aClass compile: 'thirtyTwo ^ 32'.

	aClass removeSelector: #fortyTwo.
	aClass removeSelector: #thirtyTwo.

	chunkExporter fileOut: (self allLogEntriesWith: EpMethodRemoval).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 3.
	self assert: ((chunks last: 2) allSatisfy: #isDoItDeclaration).

	methodNode1 := OCParser parseExpression: (chunks at: chunks size - 1) contents.
	methodNode2 := OCParser parseExpression: chunks last contents.

	self assert: methodNode1 selector equals: #removeSelector:.
	self 
		assertCollection: methodNode1 arguments 
		hasSameElements: { OCVariableNode named: #fortyTwo }.
		
	self assert: methodNode2 selector equals: #removeSelector:.
	self 
		assertCollection: methodNode2 arguments 
		hasSameElements: { OCVariableNode named: #thirtyTwo }.
]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutSingleClassAddition [

	| aClass outputFileReference chunks literalNodes |
	aClass := classFactory newClass.

	chunkExporter fileOut: (self allLogEntriesWith: EpClassAddition).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 2.
	self assert: chunks last isDoItDeclaration.

	literalNodes := (OCParser parseExpression: chunks last contents)
		                allChildren select: [ :each | each isLiteralNode ].

	self assert: (literalNodes anySatisfy: [ :node | node value = aClass name ]).
]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutSingleClassModification [

	| aClass outputFileReference chunks rbNode |
	aClass := classFactory newClass.
	aClass addInstVarNamed: #x.

	chunkExporter fileOut: (self allLogEntriesWith: EpClassModification).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 2.
	self assert: chunks last isDoItDeclaration.

	rbNode := (OCParser parseExpression: chunks last contents) allChildren select: #isLiteralNode.
	self assert: (rbNode anySatisfy: [ :node | node value = #x ]).
]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutSingleClassRemoval [

	| aClass outputFileReference chunks messageNode |
	aClass := classFactory newClass.
	classFactory delete: aClass.
	
	chunkExporter fileOut: (self allLogEntriesWith: EpClassRemoval).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 2.
	self assert: chunks last isDoItDeclaration.

	messageNode := OCParser parseExpression: chunks last contents.
	self assert: messageNode selector equals: #removeFromSystem.
]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutSingleMethodAddition [

	| aClass outputFileReference chunks methodNode |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^ 42'.

	chunkExporter fileOut: (self allLogEntriesWith: EpMethodAddition).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 2.
	self assert: chunks last isMethodDeclaration.

	methodNode := OCParser parseMethod: chunks last contents.
	self assert: methodNode selector equals: #fortyTwo.
	self 
		assert: methodNode body value 
		equals: (OCSequenceNode statements:{ (OCReturnNode value: (OCLiteralValueNode value: 42)) })
]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutSingleMethodModificationOfProtocol [

	| aClass outputFileReference chunks methodChunk |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^ 42' classified: 'old'.
	aClass >> #fortyTwo protocol: 'new'.

	chunkExporter fileOut: (self allLogEntriesWith: EpMethodModification).
	outputFileReference := chunkExporter outputFileReference.
	chunks := chunkImporter chunksFromFileNamed:
		          outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 2.

	methodChunk := chunks last.
	self assert: methodChunk isMethodDeclaration.
	self assert: methodChunk protocol equals: 'new'
]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutSingleMethodModificationOfSourceCode [

	| aClass outputFileReference chunks methodNode |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^ 42'.
	aClass compile: 'fortyTwo ^ 0.42'.

	chunkExporter fileOut: (self allLogEntriesWith: EpMethodModification).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 2.
	self assert: chunks last isMethodDeclaration.

	methodNode := OCParser parseMethod: chunks last contents.

	self assert: methodNode selector equals: #fortyTwo.
	self 
		assert: methodNode body value 
		equals: (OCSequenceNode statements:{ (OCReturnNode value: (OCLiteralValueNode value: 0.42)) }).

]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutSingleMethodRemoval [

	| aClass outputFileReference chunks methodNode |
	aClass := classFactory newClass.
	aClass compile: 'fortyTwo ^ 42'.
	aClass removeSelector: #fortyTwo.
	
	chunkExporter fileOut: (self allLogEntriesWith: EpMethodRemoval).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 2.
	self assert: chunks last isDoItDeclaration.

	methodNode := OCParser parseExpression: chunks last contents.
	self assert: methodNode selector equals: #removeSelector:.
	self 
		assertCollection: methodNode arguments 
		hasSameElements: { OCVariableNode named: #fortyTwo }
]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutSingleProtocolAddition [

	| aClass outputFileReference chunks literalValueNode |

	aClass := classFactory newClass.
	aClass addProtocol: 'protocol'.

	chunkExporter fileOut: (self allLogEntriesWith: EpProtocolAddition).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 2.
	self assert: chunks last isClassOrganizationDeclaration.

	literalValueNode := OCParser parseExpression: chunks last contents.
	self assert: literalValueNode value equals: #protocol.

]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutSingleProtocolRemoval [

	| aClass outputFileReference chunks methodNode |

	aClass := classFactory newClass.
	aClass addProtocol: 'protocol'.
	aClass removeProtocol: 'protocol'.

	chunkExporter fileOut: (self allLogEntriesWith: EpProtocolRemoval).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 2.
	self assert: chunks last isDoItDeclaration.

	methodNode := OCParser parseExpression: chunks last contents.
	self assert: methodNode selector equals: #removeProtocolNamed:.

]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutSingleTraitAddition [

	| aTrait outputFileReference chunks literalNodes |
	aTrait := classFactory newTrait.

	chunkExporter fileOut: (self allLogEntriesWith: EpTraitAddition).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 2.
	self assert: chunks last isDoItDeclaration.

	literalNodes := (OCParser parseExpression: chunks last contents)
		                allChildren select: [ :each | each isLiteralNode ].

	self assert: (literalNodes anySatisfy: [ :node | node value = aTrait name ]).
]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutSingleTraitModification [

	| aTrait outputFileReference chunks rbNode |
	aTrait := classFactory newTrait.
	aTrait addInstVarNamed: #x.

	chunkExporter fileOut: (self allLogEntriesWith: EpTraitModification).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 2.
	self assert: chunks last isDoItDeclaration.

	rbNode := (OCParser parseExpression: chunks last contents) allChildren select: #isLiteralNode.
	self assert: (rbNode anySatisfy: [ :node | node value = #x ]).
]

{ #category : 'tests' }
EpFileOutModificationsTest >> testFileOutSingleTraitRemoval [

	| aTrait outputFileReference chunks messageNode |
	aTrait := classFactory newTrait.
	classFactory delete: aTrait.

	chunkExporter fileOut: (self allLogEntriesWith: EpTraitRemoval).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 2.
	self assert: chunks last isDoItDeclaration.

	messageNode := OCParser parseExpression: chunks last contents.
	self assert: messageNode selector equals: #removeFromSystem.
]

{ #category : 'tests' }
EpFileOutModificationsTest >> testMethodModificationWithWideString [


	| aClass outputFileReference chunks methodNode |
	aClass := classFactory newClass.
	aClass compile: self wideStringSourceCode.
	aClass compile: self wideStringSourceCode, ' '.

	chunkExporter fileOut: (self allLogEntriesWith: EpMethodModification).
	outputFileReference := chunkExporter outputFileReference.

	chunks := chunkImporter chunksFromFileNamed: outputFileReference fullName.

	self assert: outputFileReference exists.
	self assert: chunks size equals: 2.
	self assert: chunks last isMethodDeclaration.

	methodNode := OCParser parseMethod: chunks last contents.

	self assert: methodNode selector equals: #pangrams.
	self 
		assert: methodNode
		equals: (OCParser parseMethod: self wideStringSourceCode).
]

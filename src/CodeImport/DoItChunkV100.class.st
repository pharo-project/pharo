"
I represent the declaration of some code to evaluate.  My contents are the code to evaluate.

Sending me the message #import makes me evaluate the code and return the result of the computation.

Be careful, my #import method also handles ChangeSet interaction so far, handling Changeset preambles and Postscripts.
"
Class {
	#name : 'DoItChunkV100',
	#superclass : 'CodeChunk',
	#instVars : [
		'contentsAST'
	],
	#category : 'CodeImport-Chunks',
	#package : 'CodeImport',
	#tag : 'Chunks'
}

{ #category : 'visiting' }
DoItChunkV100 >> accept: aVisitor [

	^ aVisitor visitDoItChunk: self
]

{ #category : 'importing' }
DoItChunkV100 >> contentsAST [

	^ contentsAST ifNil: [ contentsAST := OCParser parseExpression: contents ]
]

{ #category : 'accessing' }
DoItChunkV100 >> description [

	^ 'DoIt (', (contents first: (40 min: contents size)), ')'
]

{ #category : 'importing' }
DoItChunkV100 >> importWithLogSource: logSource [

	(contents beginsWith: '----') ifTrue: [ ^ self ].

	self class codeSupportAnnouncer announce: (DoItChunkImported new
			 contents: contents;
			 logSource: logSource;
			 yourself). "The package and class creation code got updated in Pharo but Monticello rely on this code. In order to not break the compatibility we are now checking if the code to import is this code and we handle it in another way."
	self isPackageAdditionChunk ifTrue: [ ^ self packageOrganizer ensurePackageMatching: self contentsAST arguments first value ].
	self isClassAdditionChunk ifTrue: [ ^ OldClassDefinitionBuilder buildFromAST: self contentsAST ].

	^ OpalEvaluator new
		logged: logSource;
		evaluate: contents
]

{ #category : 'importing' }
DoItChunkV100 >> isClassAdditionChunk [

	^ OldClassDefinitionBuilder isOldClassCreation: self contentsAST
]

{ #category : 'testing' }
DoItChunkV100 >> isDoItDeclaration [

	^ true
]

{ #category : 'testing' }
DoItChunkV100 >> isPackageAddition: ast [
	"This is for backward compatibility to be able to import code with old way of creating a package."

	^ ast isMessage and: [ ast receiver isVariable and: [ ast receiver name = #SystemOrganization and: [ ast selector = #addCategory: ] ] ]
]

{ #category : 'importing' }
DoItChunkV100 >> isPackageAdditionChunk [

	^ self isPackageAddition: self contentsAST
]

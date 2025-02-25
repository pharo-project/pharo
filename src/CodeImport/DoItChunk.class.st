"
I represent the declaration of some code to evaluate.  My contents are the code to evaluate.

Sending me the message #import makes me evaluate the code and return the result of the computation.

Be careful, my #import method also handles ChangeSet interaction so far, handling Changeset preambles and Postscripts.
"
Class {
	#name : 'DoItChunk',
	#superclass : 'CodeChunk',
	#category : 'CodeImport-Chunks',
	#package : 'CodeImport',
	#tag : 'Chunks'
}

{ #category : 'visiting' }
DoItChunk >> accept: aVisitor [

	^ aVisitor visitDoItChunk: self
]

{ #category : 'accessing' }
DoItChunk >> description [

	^ 'DoIt (', (contents first: (40 min: contents size)), ')'
]

{ #category : 'importing' }
DoItChunk >> importFor: requestor logSource: logSource [

	| ast |
	(contents beginsWith: '----') ifTrue: [ ^ self ].

	self class codeSupportAnnouncer announce: (DoItChunkImported new
			 contents: contents;
			 logSource: logSource;
			 yourself).

	"The package and class creation code got updated in Pharo but Monticello rely on this code. In order to not break the compatibility we are now checking if the code to import is this code and we handle it in another way."
	ast := OCParser parseExpression: contents.
	(self isPackageAddition: ast) ifTrue: [ ^ self packageOrganizer ensurePackageMatching: ast arguments first value ].
	(OldClassDefinitionBuilder isOldClassCreation: ast) ifTrue: [ ^ OldClassDefinitionBuilder buildFromAST: ast ].

	^ self class compiler class new
		  source: contents;
		  requestor: requestor;
		  logged: logSource;
		  evaluate
]

{ #category : 'testing' }
DoItChunk >> isDoItDeclaration [

	^ true
]

{ #category : 'testing' }
DoItChunk >> isPackageAddition: ast [
	"This is for backward compatibility to be able to import code with old way of creating a package."

	^ ast isMessage and: [ ast receiver isVariable and: [ ast receiver name = #SystemOrganization and: [ ast selector = #addCategory: ] ] ]
]

"
I represent the declaration of some code to evaluate.  My contents are the code to evaluate.

Sending me the message #import makes me evaluate the code and return the result of the computation.

Be careful, my #import method also handles ChangeSet interaction so far, handling Changeset preambles and Postscripts.
"
Class {
	#name : 'ClassChunk',
	#superclass : 'CodeChunk',
	#category : 'CodeImport-Chunks',
	#package : 'CodeImport',
	#tag : 'Chunks'
}

{ #category : 'visiting' }
ClassChunk >> accept: aVisitor [

	^ aVisitor visitClassChunk: self
]

{ #category : 'accessing' }
ClassChunk >> description [

	^ 'Class (', (contents first: (40 min: contents size)), ')'
]

{ #category : 'importing' }
ClassChunk >> importWithLogSource: logSource [

	| ast |
	ast := OCParser parseExpression: contents.
	^ ShiftClassBuilder new
		  buildFromAST: ast;
		  install
]

{ #category : 'testing' }
ClassChunk >> isClassDeclaration [

	^ true
]

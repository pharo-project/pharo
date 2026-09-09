"
I represent the declaration of some code to evaluate.  My contents are the code to evaluate.

Sending me the message #import makes me evaluate the code and return the result of the computation.

Be careful, my #import method also handles ChangeSet interaction so far, handling Changeset preambles and Postscripts.
"
Class {
	#name : 'DoItChunkV200',
	#superclass : 'CodeChunk',
	#category : 'CodeImport-Chunks',
	#package : 'CodeImport',
	#tag : 'Chunks'
}

{ #category : 'visiting' }
DoItChunkV200 >> accept: aVisitor [

	^ aVisitor visitDoItChunk: self
]

{ #category : 'accessing' }
DoItChunkV200 >> description [

	^ 'DoIt (', (contents first: (40 min: contents size)), ')'
]

{ #category : 'importing' }
DoItChunkV200 >> importWithLogSource: logSource [

	^ OpalEvaluator new
		  logged: logSource;
		  evaluate: contents
]

{ #category : 'testing' }
DoItChunkV200 >> isDoItDeclaration [

	^ true
]

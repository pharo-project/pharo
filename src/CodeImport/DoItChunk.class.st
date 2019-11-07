"
I represent the declaration of some code to evaluate.  My contents are the code to evaluate.

Sending me the message #import makes me evaluate the code and return the result of the computation.

Be careful, my #import method also handles ChangeSet interaction so far, handling Changeset preambles and Postscripts.
"
Class {
	#name : #DoItChunk,
	#superclass : #CodeChunk,
	#category : #'CodeImport-Chunks'
}

{ #category : #visiting }
DoItChunk >> accept: aVisitor [

	^ aVisitor visitDoItChunk: self
]

{ #category : #accessing }
DoItChunk >> description [

	^ 'DoIt (', (contents first: (40 min: contents size)), ')'
]

{ #category : #importing }
DoItChunk >> importFor: requestor logSource: logSource [
	
	(contents beginsWith: '----') ifTrue: [ ^self ].
	
	SystemAnnouncer uniqueInstance announce: (DoItChunkImported new
		contents: contents; 
		requestor: requestor;
		logSource: logSource;
		yourself). 
	
	^Smalltalk compiler class new
		source: contents;
		requestor: requestor;
		logged: logSource;
		evaluate.
]

{ #category : #testing }
DoItChunk >> isDoItDeclaration [

	^ true
]

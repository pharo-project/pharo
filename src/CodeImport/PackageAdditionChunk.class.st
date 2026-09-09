"
I represent the declaration of some code to evaluate.  My contents are the code to evaluate.

Sending me the message #import makes me evaluate the code and return the result of the computation.

Be careful, my #import method also handles ChangeSet interaction so far, handling Changeset preambles and Postscripts.
"
Class {
	#name : 'PackageAdditionChunk',
	#superclass : 'CodeChunk',
	#category : 'CodeImport-Chunks',
	#package : 'CodeImport',
	#tag : 'Chunks'
}

{ #category : 'visiting' }
PackageAdditionChunk >> accept: aVisitor [

	^ aVisitor visitPackageAddition: self
]

{ #category : 'accessing' }
PackageAdditionChunk >> description [

	^ 'PackageAddition (', (contents first: (40 min: contents size)), ')'
]

{ #category : 'importing' }
PackageAdditionChunk >> importWithLogSource: logSource [
	"SystemOrganization addCategory: #aCategory"

	| ast |
	ast := OCParser parseExpression: contents.
	self packageOrganizer ensurePackageMatching: ast arguments first value
]

{ #category : 'testing' }
PackageAdditionChunk >> isPackageDeclaration [

	^ true
]

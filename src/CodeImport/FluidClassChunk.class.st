Class {
	#name : #FluidClassChunk,
	#superclass : #CodeChunk,
	#category : #'CodeImport-Chunks'
}

{ #category : #visiting }
FluidClassChunk >> accept: aVisitor [

	^ aVisitor visitFluidClassChunk: self
]

{ #category : #accessing }
FluidClassChunk >> description [
	^ contents
]

{ #category : #importing }
FluidClassChunk >> importFor: requestor logSource: logSource [ 


	SystemAnnouncer uniqueInstance announce: (DoItChunkImported new
		contents: contents;
		requestor: requestor;
		logSource: logSource;
		yourself).
	"We raise the same event because apparently there is only one."
	
	"we get a build so we ask it to install itself"
	^ (Smalltalk compiler class new
		source: contents;
		requestor: requestor;
		logged: logSource;
		evaluate) install
]

"
I'm the declaration of a style stuff into a file.  I'm not really used, but I'm here to make parsing an homogeneous process and backwards compatibility.

Sending me the #import message does nothing :).
"
Class {
	#name : #StyleChunk,
	#superclass : #CodeChunk,
	#category : #'CodeImport-Chunks'
}

{ #category : #visiting }
StyleChunk >> accept: aVisitor [

	^ aVisitor visitStyleChunk: self
]

{ #category : #accessing }
StyleChunk >> description [

	^ 'Style'
]

{ #category : #importing }
StyleChunk >> importFor: aRequestor logSource: logSource [
	"Nothing to do with style declarations"
]

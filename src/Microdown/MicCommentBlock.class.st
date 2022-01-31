"
A commented line in Pillar is a line starting with %.
A comment does not flow on multiple lines. 
Each line to be commented should start with %.

I should be a subclass of SingleLineBlock and not continuousMarkedBlock.
"
Class {
	#name : #MicCommentBlock,
	#superclass : #MicContinuousMarkedBlock,
	#category : #'Microdown-Model'
}

{ #category : #visiting }
MicCommentBlock >> accept: aVisitor [
 	^ aVisitor visitComment: self
]

{ #category : #markups }
MicCommentBlock >> lineMarkup [

	^ parser commentedLineMarkup 
]

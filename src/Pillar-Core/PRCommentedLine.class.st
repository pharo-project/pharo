"
A commented line in Pillar is a line starting with %.
A comment does not flows on multiple lines. 
Each line to be commented should start with %.
"
Class {
	#name : #PRCommentedLine,
	#superclass : #PRText,
	#category : #'Pillar-Core-Model'
}

{ #category : #visiting }
PRCommentedLine >> accept: aVisitor [
	^ aVisitor visitCommentedLine: self
]

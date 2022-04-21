"
A message node
"
Class {
	#name : #RFMessageNode,
	#superclass : #ASTMessageNode,
	#category : #'Reflectivity-Compiler'
}

{ #category : #debugging }
RFMessageNode >> debugHighlightRange [
	^ parent debugHighlightRange
]

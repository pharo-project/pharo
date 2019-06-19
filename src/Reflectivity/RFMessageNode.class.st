"
A message node
"
Class {
	#name : #RFMessageNode,
	#superclass : #RBMessageNode,
	#category : #'Reflectivity-Compiler'
}

{ #category : #debugging }
RFMessageNode >> debugHighlightRange [
	^ parent debugHighlightRange 
]

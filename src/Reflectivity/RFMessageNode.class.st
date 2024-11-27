"
A message node
"
Class {
	#name : 'RFMessageNode',
	#superclass : 'ASTMessageNode',
	#category : 'Reflectivity-Compiler',
	#package : 'Reflectivity',
	#tag : 'Compiler'
}

{ #category : 'debugging' }
RFMessageNode >> debugHighlightRange [
	^ parent debugHighlightRange
]

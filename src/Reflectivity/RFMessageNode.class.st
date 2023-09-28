"
A message node
"
Class {
	#name : 'RFMessageNode',
	#superclass : 'RBMessageNode',
	#category : 'Reflectivity-Compiler',
	#package : 'Reflectivity',
	#tag : 'Compiler'
}

{ #category : 'debugging' }
RFMessageNode >> debugHighlightRange [
	^ parent debugHighlightRange
]

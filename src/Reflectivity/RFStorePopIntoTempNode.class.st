"
I define a temp that I can store into
"
Class {
	#name : 'RFStorePopIntoTempNode',
	#superclass : 'RBVariableNode',
	#category : 'Reflectivity-Compiler',
	#package : 'Reflectivity',
	#tag : 'Compiler'
}

{ #category : 'visiting' }
RFStorePopIntoTempNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitStorePopIntoTempNode: self
]

"
I define a temp that I can store into
"
Class {
	#name : #RFStorePopIntoTempNode,
	#superclass : #RBVariableNode,
	#category : #'Reflectivity-Compiler'
}

{ #category : #visiting }
RFStorePopIntoTempNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitStorePopIntoTempNode: self
]

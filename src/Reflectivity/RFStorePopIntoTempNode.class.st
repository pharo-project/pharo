"
I define a temp that I can store into
"
Class {
	#name : #RFStorePopIntoTempNode,
	#superclass : #ASTVariableNode,
	#category : #'Reflectivity-Compiler'
}

{ #category : #visiting }
RFStorePopIntoTempNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitStorePopIntoTempNode: self
]

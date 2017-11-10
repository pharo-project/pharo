"
I define a temp that I can store into
"
Class {
	#name : #RFStoreIntoTempNode,
	#superclass : #RBVariableNode,
	#category : #Reflectivity-Compiler
}

{ #category : #visiting }
RFStoreIntoTempNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitStoreIntoTempNode: self
]

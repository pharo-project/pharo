"
I define a temp that I can store into
"
Class {
	#name : 'RFStoreIntoTempNode',
	#superclass : 'OCVariableNode',
	#category : 'Reflectivity-Compiler',
	#package : 'Reflectivity',
	#tag : 'Compiler'
}

{ #category : 'visiting' }
RFStoreIntoTempNode >> acceptVisitor: aProgramNodeVisitor [
	^ aProgramNodeVisitor visitStoreIntoTempNode: self
]

"
I am a binding in a Workspace. Used for non-defined temps
"
Class {
	#name : #WorkspaceVariable,
	#superclass : #LiteralVariable,
	#category : #'Slot-Core-Variables'
}

{ #category : #'code generation' }
WorkspaceVariable >> emitStore: methodBuilder [

	methodBuilder storeIntoLiteralVariable: self.


]

{ #category : #'code generation' }
WorkspaceVariable >> emitValue: methodBuilder [

	methodBuilder pushLiteralVariable: self.
]

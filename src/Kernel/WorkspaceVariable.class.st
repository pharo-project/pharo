"
I am a binding in a Workspace. Used for non-defined temps
"
Class {
	#name : #WorkspaceVariable,
	#superclass : #LiteralVariable,
	#category : #'Kernel-Variables'
}

{ #category : #testing }
WorkspaceVariable >> allowsShadowing [
	^true
]

{ #category : #'code generation' }
WorkspaceVariable >> emitStore: methodBuilder [

	methodBuilder storeIntoLiteralVariable: self.


]

{ #category : #'code generation' }
WorkspaceVariable >> emitValue: methodBuilder [

	methodBuilder pushLiteralVariable: self
]

{ #category : #testing }
WorkspaceVariable >> isReferenced [
	^ false
]

{ #category : #testing }
WorkspaceVariable >> isWorkspaceVariable [
	^ true
]

"
I am a refactory change for a instance variable renaming. The RBRefactory api is implemented in my super class, I just define
the concrete ""instance variable"" renaming.
"
Class {
	#name : #RBRenameInstanceVariableChange,
	#superclass : #RBRenameVariableChange,
	#category : #'Refactoring-Changes'
}

{ #category : #private }
RBRenameInstanceVariableChange >> addNewVariable [
	self flag: #todo.
	"We should use the factory"
	(RBAddInstanceVariableChange add: newName to: self changeClass)
		execute
]

{ #category : #private }
RBRenameInstanceVariableChange >> copyOldValuesToNewVariable [
	| newIndex oldIndex |
	oldIndex := self changeClass allInstVarNames indexOf: oldName asString.
	newIndex := self changeClass allInstVarNames indexOf: newName asString.
	self changeClass withAllSubclasses do: [ :class |
		class allInstances 
			do: [ :each | each instVarAt: newIndex put: (each instVarAt: oldIndex) ] ]
]

{ #category : #printing }
RBRenameInstanceVariableChange >> printOn: aStream [
	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' removeInstVarName: ';
		print: self oldName;
		nextPut: $!; cr.
	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' addInstVarName: ';
		print: self newName;
		nextPut: $!
]

{ #category : #private }
RBRenameInstanceVariableChange >> removeOldVariable [
	self flag: #todo.
	"We should use the factory"
	(RBRemoveInstanceVariableChange remove: oldName from: self changeClass) 
		execute
]

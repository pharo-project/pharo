"
I am a refactory change for a instance variable renaming. The RBRefactory api is implemented in my super class, I just define
the concrete ""instance variable"" renaming.
"
Class {
	#name : 'ReRenameInstanceVariableChange',
	#superclass : 'ReRenameVariableChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'private' }
ReRenameInstanceVariableChange >> addNewVariable [

	(self changeClass allInstVarNames includes: newName)
		ifFalse: [ (changeFactory addInstanceVariable: newName to: self changeClass) execute ]
]

{ #category : 'private' }
ReRenameInstanceVariableChange >> asText [

	^ self printString
]

{ #category : 'private' }
ReRenameInstanceVariableChange >> copyOldValuesToNewVariable [

	self changeClass withAllSubclasses do: [ :class |
		class allInstances
			do: [ :each | each instVarNamed: newName asString put: (each instVarNamed: oldName asString) ] ]
]

{ #category : 'printing' }
ReRenameInstanceVariableChange >> printOn: aStream [

	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' removeInstVarName: ';
		print: self oldName;
		cr.
	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' addInstVarName: ';
		print: self newName
]

{ #category : 'private' }
ReRenameInstanceVariableChange >> removeOldVariable [

	(self changeClass allInstVarNames includes: oldName) ifTrue: [
		(changeFactory
			 removeInstanceVariable: oldName
			 from: self changeClass) execute ]
]

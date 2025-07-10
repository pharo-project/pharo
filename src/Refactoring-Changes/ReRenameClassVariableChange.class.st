"
I am a refactory change for a class variable renaming. The RBRefactory api is implemented in my super class, I just define
the concrete ""classvariable"" renaming.
"
Class {
	#name : 'ReRenameClassVariableChange',
	#superclass : 'ReRenameVariableChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'private' }
ReRenameClassVariableChange >> addNewVariable [
	(changeFactory addClassVariable: newName to: self changeClass)
		execute
]

{ #category : 'private' }
ReRenameClassVariableChange >> copyOldValuesToNewVariable [

	| oldValue |
	oldValue := self changeClass classPool at: oldName ifAbsent: [ nil ].
	self changeClass classPool at: newName asSymbol put: oldValue
]

{ #category : 'printing' }
ReRenameClassVariableChange >> printOn: aStream [

	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' removeClassVarName: ';
		print: self oldName;
		cr.
	aStream
		nextPutAll: self displayClassName;
		nextPutAll: ' addClassVarName: ';
		print: self newName
]

{ #category : 'private' }
ReRenameClassVariableChange >> removeOldVariable [
	(changeFactory removeClassVariable: oldName from: self changeClass)
		execute
]

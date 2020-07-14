"
I am an abstract PREditorFile, to distinguish a **new** text file and an existing **opened** text file.
"
Class {
	#name : #PRPearFile,
	#superclass : #Object,
	#instVars : [
		'file'
	],
	#category : #'Pillar-Pear'
}

{ #category : #accessing }
PRPearFile >> basename [
	self subclassResponsibility 
]

{ #category : #accessing }
PRPearFile >> file [
	^ file
]

{ #category : #accessing }
PRPearFile >> file: aFile [
	file := aFile
]

{ #category : #accessing }
PRPearFile >> fullName [
	self subclassResponsibility 
]

{ #category : #testing }
PRPearFile >> isNew [
	self subclassResponsibility 
]

{ #category : #action }
PRPearFile >> save: aText withExtension: anExtension [
	^ self subclassResponsibility 
]

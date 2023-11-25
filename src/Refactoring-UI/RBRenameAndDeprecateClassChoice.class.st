Class {
	#name : 'RBRenameAndDeprecateClassChoice',
	#superclass : 'RBDeprecateClassChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'execution' }
RBRenameAndDeprecateClassChoice >> action [

	driver renameAndDeprecate 
]

{ #category : 'accessing' }
RBRenameAndDeprecateClassChoice >> description [

	^ 'Rename this class, subclass it with class that has old name and deprecate that class'
]

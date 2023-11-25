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

	^ 'Rename this class, subclass it with a new class that has the old name of the class and deprecate that class'
]

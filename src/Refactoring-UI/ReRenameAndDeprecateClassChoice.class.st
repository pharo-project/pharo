Class {
	#name : 'ReRenameAndDeprecateClassChoice',
	#superclass : 'ReDeprecateClassChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'execution' }
ReRenameAndDeprecateClassChoice >> action [

	driver renameAndDeprecate 
]

{ #category : 'accessing' }
ReRenameAndDeprecateClassChoice >> description [

	^ 'Rename this class, subclass it with a new class that has the old name of the class and deprecate that class'
]

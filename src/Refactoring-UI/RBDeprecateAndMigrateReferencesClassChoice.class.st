Class {
	#name : 'RBDeprecateAndMigrateReferencesClassChoice',
	#superclass : 'RBDeprecateClassChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'execution' }
RBDeprecateAndMigrateReferencesClassChoice >> action [

	driver deprecateAndMigrateReferences
]

{ #category : 'accessing' }
RBDeprecateAndMigrateReferencesClassChoice >> description [

	^ 'Deprecate class and migrate all references to another class'
]

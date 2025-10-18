Class {
	#name : 'ReDeprecateAndMigrateReferencesClassChoice',
	#superclass : 'ReDeprecateClassChoice',
	#category : 'Refactoring-UI-Choices',
	#package : 'Refactoring-UI',
	#tag : 'Choices'
}

{ #category : 'execution' }
ReDeprecateAndMigrateReferencesClassChoice >> action [

	driver deprecateAndMigrateReferences
]

{ #category : 'accessing' }
ReDeprecateAndMigrateReferencesClassChoice >> description [

	^ 'Deprecate class and migrate all references to another class'
]

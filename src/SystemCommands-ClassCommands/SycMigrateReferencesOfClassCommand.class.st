"
I am a command to migrate references of a given class to another.

Internal Representation and Key Implementation Points.

Instance Variables
newName:  <String>
"
Class {
	#name : #SycMigrateReferencesOfClassCommand,
	#superclass : #SycSingleClassCommand,
	#instVars : [
		'newName'
	],
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #converting }
SycMigrateReferencesOfClassCommand >> asRefactorings [
	| refactoring |
	refactoring :=  RBDeprecateClassRefactoring deprecate: targetClass in: newName name.
	refactoring shouldFixSubclasses: false.
	refactoring shouldCopyExtensions: false.
	refactoring shouldRemoveExtensions: false.
	^ { refactoring }
]

{ #category : #accessing }
SycMigrateReferencesOfClassCommand >> defaultMenuIconName [
	^ #source
]

{ #category : #accessing }
SycMigrateReferencesOfClassCommand >> defaultMenuItemName [
	^'Migrate references'
]

{ #category : #execution }
SycMigrateReferencesOfClassCommand >> execute [

	self asRefactorings do: [ :ref | ref execute ]
]

{ #category : #testing }
SycMigrateReferencesOfClassCommand >> isComplexRefactoring [
	^ true
]

{ #category : #execution }
SycMigrateReferencesOfClassCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	newName := aToolContext browser searchDialog
		requestSingleObject: 'Choose new class'
		from: ClyAllClassesQuery sorted.
	newName ifNil: [ CmdCommandAborted signal]
]

"
I am a command to migrate references of a given class to another.

Internal Representation and Key Implementation Points.

Instance Variables
newName:  <String>
"
Class {
	#name : 'SycMigrateReferencesOfClassCommand',
	#superclass : 'SycSingleClassCommand',
	#instVars : [
		'newName'
	],
	#category : 'SystemCommands-ClassCommands',
	#package : 'SystemCommands-ClassCommands'
}

{ #category : 'execution' }
SycMigrateReferencesOfClassCommand >> execute [

	self asRefactorings do: [ :ref | ref execute ]
]

{ #category : 'accessing' }
SycMigrateReferencesOfClassCommand >> iconName [
	^ #source
]

{ #category : 'testing' }
SycMigrateReferencesOfClassCommand >> isComplexRefactoring [
	^ true
]

{ #category : 'accessing' }
SycMigrateReferencesOfClassCommand >> name [
	^'Migrate references'
]

{ #category : 'execution' }
SycMigrateReferencesOfClassCommand >> prepareFullExecutionInContext: aToolContext [

	super prepareFullExecutionInContext: aToolContext.
	newName := aToolContext requestSingleClass: 'Choose new class'
]

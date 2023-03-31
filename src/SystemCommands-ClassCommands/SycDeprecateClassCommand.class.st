"
I am a command to deprecate given class.

Internal Representation and Key Implementation Points.

    Instance Variables
	newName:		<String>
"
Class {
	#name : #SycDeprecateClassCommand,
	#superclass : #SycSingleClassCommand,
	#instVars : [
		'newName'
	],
	#category : #'SystemCommands-ClassCommands'
}

{ #category : #testing }
SycDeprecateClassCommand class >> canBeExecutedInContext: aToolContext [

	^ (super canBeExecutedInContext: aToolContext) and: [
		  aToolContext lastSelectedClass isDeprecated not ]
]

{ #category : #converting }
SycDeprecateClassCommand >> asRefactorings [
	| refactoring |
	refactoring := RBDeprecateClassRefactoring deprecate: targetClass in: newName name.
	^ { refactoring }
]

{ #category : #accessing }
SycDeprecateClassCommand >> defaultMenuIconName [
	^ #bug
]

{ #category : #accessing }
SycDeprecateClassCommand >> defaultMenuItemName [
	^'Deprecate'
]

{ #category : #execution }
SycDeprecateClassCommand >> execute [

	self asRefactorings do: [ :ref | ref execute ]
]

{ #category : #testing }
SycDeprecateClassCommand >> isComplexRefactoring [
	^ true
]

{ #category : #execution }
SycDeprecateClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	newName := aToolContext browser searchDialog
		requestSingleObject: 'Choose new class'
		from: ClyAllClassesQuery sorted.
	newName ifNil: [ CmdCommandAborted signal]
]

"
I am a command to show local method version.

Local version has not relation to actual source code system. Versions are retrieved form changes file.

Internal Representation and Key Implementation Points.

    Instance Variables
	method:		<CompiledMethod>
"
Class {
	#name : #SycShowMethodVersionCommand,
	#superclass : #CmdCommand,
	#instVars : [
		'method'
	],
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #testing }
SycShowMethodVersionCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isMethodSelected
]

{ #category : #accessing }
SycShowMethodVersionCommand >> defaultMenuIconName [
	^#history
]

{ #category : #accessing }
SycShowMethodVersionCommand >> defaultMenuItemName [
	^'Versions'
]

{ #category : #execution }
SycShowMethodVersionCommand >> execute [
	Smalltalk tools versionBrowser browseVersionsForMethod: method
]

{ #category : #execution }
SycShowMethodVersionCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	method := aToolContext lastSelectedMethod
]

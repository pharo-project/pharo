"
I am a command to request method tag from the user which should be used to tag editing method
"
Class {
	#name : #ClyRequestMethodTagForEditorCommand,
	#superclass : #ClyMethodEditorCommand,
	#instVars : [
		'menuItemName'
	],
	#category : #'Calypso-SystemTools-Core-Editors-Methods'
}

{ #category : #activation }
ClyRequestMethodTagForEditorCommand class >> methodEditorShorcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $e meta, $t meta for: ClyTextEditorContext
]

{ #category : #accessing }
ClyRequestMethodTagForEditorCommand >> defaultMenuItemName [

	^ menuItemName
]

{ #category : #execution }
ClyRequestMethodTagForEditorCommand >> execute [
	| tagEditor |
	tagEditor := methodEditor tagsAndPackageEditor.
	tagEditor requestChangeBy: [ tagEditor requestTag] 
]

{ #category : #'as yet unclassified' }
ClyRequestMethodTagForEditorCommand >> readParametersFromContext: aToolContext [

	menuItemName := aToolContext tool hasUnacceptedEdits 
		ifTrue: [ 'Classify currently editing method' ]
		ifFalse: [ 'Classify method' ]

	
]

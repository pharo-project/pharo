"
I am a command to request a protocol from the user which should be used to reclassify a method
"
Class {
	#name : #ClyRequestProtocolForEditorCommand,
	#superclass : #ClyMethodEditorCommand,
	#instVars : [
		'menuItemName'
	],
	#category : #'Calypso-SystemTools-Core-Editors-Methods'
}

{ #category : #activation }
ClyRequestProtocolForEditorCommand class >> methodEditorShorcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $e meta, $t meta for: ClyTextEditorContext
]

{ #category : #accessing }
ClyRequestProtocolForEditorCommand >> defaultMenuItemName [

	^ menuItemName
]

{ #category : #execution }
ClyRequestProtocolForEditorCommand >> execute [
	| tagEditor |
	tagEditor := methodEditor tagsAndPackageEditor.
	tagEditor requestChangeBy: [ tagEditor requestProtocol]
]

{ #category : #execution }
ClyRequestProtocolForEditorCommand >> readParametersFromContext: aToolContext [

	menuItemName := aToolContext tool hasUnacceptedEdits
		ifTrue: [ 'Classify currently editing method' ]
		ifFalse: [ 'Classify method' ]
]

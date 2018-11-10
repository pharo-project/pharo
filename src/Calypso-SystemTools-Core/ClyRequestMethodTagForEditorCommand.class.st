"
I am a command to request method tag from the user which should be used to tag editing method
"
Class {
	#name : #ClyRequestMethodTagForEditorCommand,
	#superclass : #ClyMethodEditorCommand,
	#category : #'Calypso-SystemTools-Core-Editors-Methods'
}

{ #category : #activation }
ClyRequestMethodTagForEditorCommand class >> methodEditorShorcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $e meta, $t meta for: ClyTextEditorContext
]

{ #category : #accessing }
ClyRequestMethodTagForEditorCommand >> defaultMenuItemName [
	^'Classify method'
]

{ #category : #execution }
ClyRequestMethodTagForEditorCommand >> execute [
	| tagEditor |
	tagEditor := methodEditor tagsAndPackageEditor.
	tagEditor requestChangeBy: [ tagEditor requestTag] 
]

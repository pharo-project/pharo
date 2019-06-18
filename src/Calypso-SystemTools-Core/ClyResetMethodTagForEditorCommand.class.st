Class {
	#name : #ClyResetMethodTagForEditorCommand,
	#superclass : #ClyMethodEditorCommand,
	#category : #'Calypso-SystemTools-Core-Editors-Methods'
}

{ #category : #activation }
ClyResetMethodTagForEditorCommand class >> methodEditorShorcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $e meta, $r meta for: ClyTextEditorContext
]

{ #category : #accessing }
ClyResetMethodTagForEditorCommand >> defaultMenuItemName [

	^ 'Unclassify method'
]

{ #category : #execution }
ClyResetMethodTagForEditorCommand >> execute [

	| tagEditor |
	tagEditor := methodEditor tagsAndPackageEditor.
	tagEditor resetTagsAndPackage 
]

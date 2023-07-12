Class {
	#name : #ClyResetProtocolForEditorCommand,
	#superclass : #ClyMethodEditorCommand,
	#category : #'Calypso-SystemTools-Core-Editors-Methods'
}

{ #category : #activation }
ClyResetProtocolForEditorCommand class >> methodEditorShorcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $e meta, $r meta for: ClyTextEditorContext
]

{ #category : #accessing }
ClyResetProtocolForEditorCommand >> defaultMenuItemName [

	^ 'Unclassify method'
]

{ #category : #execution }
ClyResetProtocolForEditorCommand >> execute [

	methodEditor tagsAndPackageEditor resetProtocolAndPackage
]

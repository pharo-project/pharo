"
I am a command that unclasifies a method.
"
Class {
	#name : 'ClyResetProtocolForEditorCommand',
	#superclass : 'ClyMethodEditorCommand',
	#category : 'Calypso-SystemTools-Core-Editors-Methods',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Methods'
}

{ #category : 'activation' }
ClyResetProtocolForEditorCommand class >> methodEditorShorcutActivation [
	<classAnnotation>

	^CmdShortcutActivation by: $e meta, $u meta for: ClyTextEditorContext
]

{ #category : 'accessing' }
ClyResetProtocolForEditorCommand >> defaultMenuItemName [

	^ 'Unclassify method'
]

{ #category : 'accessing' }
ClyResetProtocolForEditorCommand >> description [

	^ 'Unclasifies a method.'
]

{ #category : 'execution' }
ClyResetProtocolForEditorCommand >> execute [

	methodEditor protocolAndPackageEditor resetProtocolAndPackage
]

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

	^ClyShortcutActivation by: $e meta, $u meta for: ClyTextEditorContext
]

{ #category : 'accessing' }
ClyResetProtocolForEditorCommand >> description [

	^ 'Unclasifies a method.'
]

{ #category : 'execution' }
ClyResetProtocolForEditorCommand >> executeRefactoring [

	methodEditor protocolAndPackageEditor resetProtocolAndPackage
]

{ #category : 'accessing' }
ClyResetProtocolForEditorCommand >> name [

	^ 'Unclassify method'
]

{ #category : 'accessing' }
ClyResetProtocolForEditorCommand >> order [

	^ 1000
]

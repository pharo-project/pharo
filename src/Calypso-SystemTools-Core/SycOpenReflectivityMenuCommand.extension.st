Extension { #name : #SycOpenReflectivityMenuCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycOpenReflectivityMenuCommand class >> methodEditorShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $r meta shift for: ClySourceCodeContext
]

{ #category : #'*Calypso-SystemTools-Core' }
SycOpenReflectivityMenuCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 0.3 for: ClySourceCodeContext
]

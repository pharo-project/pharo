Extension { #name : #SycInspectVariableCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycInspectVariableCommand class >> fullBrowserDoubleClickActivation [
	<classAnnotation>
	
	^(CmdDoubleClickActivation for: ClyVariableContextOfFullBrowser)
		priority: 10000
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycInspectVariableCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byItemOf: ClyQueryMenuGroup for: ClyVariableContextOfFullBrowser 
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycInspectVariableCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^(CmdShortcutActivation 
		by: PharoShortcuts current inspectItShortcut for: ClyVariableContextOfFullBrowser)
			priority: 100000
]

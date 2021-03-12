Extension { #name : #SycMergeVariableCommand }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycMergeVariableCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $v meta, $m meta for: ClyFullBrowserVariableContext
]

{ #category : #'*Calypso-SystemTools-FullBrowser' }
SycMergeVariableCommand class >> sourceCodeShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $v meta, $m meta for: ClySourceCodeContext 
]

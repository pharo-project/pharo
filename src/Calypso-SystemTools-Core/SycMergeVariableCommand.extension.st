Extension { #name : #SycMergeVariableCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycMergeVariableCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 1.1 for: ClyFullBrowserVariableContext 
]

{ #category : #'*Calypso-SystemTools-Core' }
SycMergeVariableCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.1 for: ClySourceCodeContext
]

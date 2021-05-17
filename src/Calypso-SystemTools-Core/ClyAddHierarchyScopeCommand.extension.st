Extension { #name : #ClyAddHierarchyScopeCommand }

{ #category : #'*Calypso-SystemTools-Core' }
ClyAddHierarchyScopeCommand class >> browserContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemOrder: 1.5 for: ClyClass asCalypsoItemContext  
]

{ #category : #'*Calypso-SystemTools-Core' }
ClyAddHierarchyScopeCommand class >> sourceCodeMenuActivation [
	<classAnnotation>
	
	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClySourceCodeContext
]

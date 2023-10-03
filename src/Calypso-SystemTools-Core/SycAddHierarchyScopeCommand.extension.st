Extension { #name : 'SycAddHierarchyScopeCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycAddHierarchyScopeCommand class >> browserContextMenuActivation [
	<classAnnotation>

	^ CmdContextMenuActivation byRootGroupItemOrder: 1.5 for: ClyClass asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycAddHierarchyScopeCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^ SycSourceCodeMenuActivation byRootGroupItemOrder: 1.2 for: ClySourceCodeContext
]

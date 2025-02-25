Extension { #name : 'SycRemoveMethodsInHierarchyCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycRemoveMethodsInHierarchyCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 100 for: ClyMethod asCalypsoItemContext
]

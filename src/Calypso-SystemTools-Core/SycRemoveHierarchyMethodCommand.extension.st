Extension { #name : #SycRemoveHierarchyMethodCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycRemoveHierarchyMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 100 for: ClyMethod asCalypsoItemContext
]

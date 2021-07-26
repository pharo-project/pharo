Extension { #name : #SycReplaceMessageCommand }

{ #category : #'*Calypso-SystemTools-Core' }
SycReplaceMessageCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^ SycMethodMenuActivation byRootGroupItemOrder: 1100 for: ClyMethod asCalypsoItemContext
]

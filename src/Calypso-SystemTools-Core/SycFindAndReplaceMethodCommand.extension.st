Extension { #name : 'SycFindAndReplaceMethodCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycFindAndReplaceMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation
		byRootGroupItemOrder: 25 for: ClyMethod asCalypsoItemContext
]

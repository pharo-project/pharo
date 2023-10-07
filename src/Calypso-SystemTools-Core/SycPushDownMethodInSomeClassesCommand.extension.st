Extension { #name : 'SycPushDownMethodInSomeClassesCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycPushDownMethodInSomeClassesCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 21 for: ClyMethod asCalypsoItemContext
]

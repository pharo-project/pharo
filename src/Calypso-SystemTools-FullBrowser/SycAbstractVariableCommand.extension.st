Extension { #name : 'SycAbstractVariableCommand' }

{ #category : '*Calypso-SystemTools-FullBrowser' }
SycAbstractVariableCommand class >> fullBrowserMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemFor: ClyFullBrowserVariableContext
]

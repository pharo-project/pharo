Extension { #name : 'SycOpenReflectivityMenuCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycOpenReflectivityMenuCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 0.3 for: ClySourceCodeContext
]

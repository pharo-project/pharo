Extension { #name : 'SycBrowseClassCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycBrowseClassCommand class >> browserContextMenuActivation [ 

	^ CmdContextMenuActivation 
		byRootGroupItemOrder: 1 
		for: ClyClass asCalypsoItemContext 
]

{ #category : '*Calypso-SystemTools-Core' }
SycBrowseClassCommand class >> sourceCodeMenuActivation [
	<classAnnotation>

	^SycSourceCodeMenuActivation byItemOf: ClyQueryMenuGroup for: ClySourceCodeContext
]

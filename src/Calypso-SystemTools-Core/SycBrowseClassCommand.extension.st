Extension { #name : 'SycBrowseClassCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycBrowseClassCommand class >> browserContextMenuActivation [ 

	^ ClyContextMenuActivation 
		byRootGroupItemOrder: 1 
		for: ClyClass asCalypsoItemContext 
]

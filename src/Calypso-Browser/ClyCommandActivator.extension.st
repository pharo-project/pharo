Extension { #name : 'ClyCommandActivator' }

{ #category : '*Calypso-Browser' }
ClyCommandActivator >> decorateTableCell: anItemCellMorph [

	self command decorateTableCell: anItemCellMorph using: self
]

{ #category : '*Calypso-Browser' }
ClyCommandActivator >> isCommandAppliedToBrowser [

	^ self command isAppliedToBrowser
]

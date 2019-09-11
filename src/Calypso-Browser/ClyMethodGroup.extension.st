Extension { #name : #ClyMethodGroup }

{ #category : #'*Calypso-Browser' }
ClyMethodGroup >> decorateTableCell: anItemCellMorph of: groupItem [
	super decorateTableCell: anItemCellMorph of: groupItem.
	
	methodQuery decorateMethodGroupTableCell: anItemCellMorph of: groupItem
]

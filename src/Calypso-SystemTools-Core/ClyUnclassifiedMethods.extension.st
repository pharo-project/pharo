Extension { #name : #ClyUnclassifiedMethods }

{ #category : #'*Calypso-SystemTools-Core' }
ClyUnclassifiedMethods >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label color: Color red
]

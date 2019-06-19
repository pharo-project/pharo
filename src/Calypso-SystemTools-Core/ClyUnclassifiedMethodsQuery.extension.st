Extension { #name : #ClyUnclassifiedMethodsQuery }

{ #category : #'*Calypso-SystemTools-Core' }
ClyUnclassifiedMethodsQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label color: Color red
]

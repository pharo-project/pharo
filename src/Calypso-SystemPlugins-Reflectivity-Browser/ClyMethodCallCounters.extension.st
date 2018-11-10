Extension { #name : #ClyMethodCallCounters }

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClyMethodCallCounters >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label color: Color red.
	anItemCellMorph definitionIcon: #classIcon
]

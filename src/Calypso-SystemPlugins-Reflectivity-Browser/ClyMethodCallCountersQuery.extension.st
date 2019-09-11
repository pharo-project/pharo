Extension { #name : #ClyMethodCallCountersQuery }

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClyMethodCallCountersQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label color: Color red.
	anItemCellMorph definitionIcon: #classIcon
]

Extension { #name : #ClyMethodWatchpoints }

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClyMethodWatchpoints >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label color: Color red.
	anItemCellMorph definitionIcon: #haloView
]

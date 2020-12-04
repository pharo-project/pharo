Extension { #name : #ClyMethodWatchQuery }

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClyMethodWatchQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label color: Color red.
	anItemCellMorph definitionIcon: #haloView
]

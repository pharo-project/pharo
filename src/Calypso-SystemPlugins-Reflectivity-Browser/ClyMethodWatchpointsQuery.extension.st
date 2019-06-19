Extension { #name : #ClyMethodWatchpointsQuery }

{ #category : #'*Calypso-SystemPlugins-Reflectivity-Browser' }
ClyMethodWatchpointsQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label color: Color red.
	anItemCellMorph definitionIcon: #haloView
]

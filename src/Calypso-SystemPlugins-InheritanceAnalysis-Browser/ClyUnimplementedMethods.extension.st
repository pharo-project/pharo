Extension { #name : #ClyUnimplementedMethods }

{ #category : #'*Calypso-SystemPlugins-InheritanceAnalysis-Browser' }
ClyUnimplementedMethods >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label color: Color red
	
]

{ #category : #'*Calypso-SystemPlugins-InheritanceAnalysis-Browser' }
ClyUnimplementedMethods class >> decorateTableCell: anItemCellMorph of: groupItem [
	
	anItemCellMorph label color: Color red
]

Extension { #name : #ClyUnimplementedMethodsQuery }

{ #category : #'*Calypso-SystemPlugins-InheritanceAnalysis-Browser' }
ClyUnimplementedMethodsQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label color: Color red
	
]

{ #category : #'*Calypso-SystemPlugins-InheritanceAnalysis-Browser' }
ClyUnimplementedMethodsQuery class >> decorateTableCell: anItemCellMorph of: groupItem [
	
	anItemCellMorph label color: Color red
]

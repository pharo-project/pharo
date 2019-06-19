Extension { #name : #ClyUndeclaredMethodsQuery }

{ #category : #'*Calypso-SystemPlugins-Undeclared-Browser' }
ClyUndeclaredMethodsQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label color: Color red.
	anItemCellMorph definitionIcon: #smallDebug
	
]

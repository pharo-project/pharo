Extension { #name : #ClyUndeclaredMethods }

{ #category : #'*Calypso-SystemPlugins-Undeclared-Browser' }
ClyUndeclaredMethods >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label color: Color red.
	anItemCellMorph definitionIcon: #smallDebug
	
]

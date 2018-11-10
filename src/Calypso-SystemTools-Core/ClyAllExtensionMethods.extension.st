Extension { #name : #ClyAllExtensionMethods }

{ #category : #'*Calypso-SystemTools-Core' }
ClyAllExtensionMethods >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph definitionIcon: #protocolExtensionIcon
]

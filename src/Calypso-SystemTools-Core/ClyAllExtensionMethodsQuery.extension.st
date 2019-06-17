Extension { #name : #ClyAllExtensionMethodsQuery }

{ #category : #'*Calypso-SystemTools-Core' }
ClyAllExtensionMethodsQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph definitionIcon: #protocolExtensionIcon
]

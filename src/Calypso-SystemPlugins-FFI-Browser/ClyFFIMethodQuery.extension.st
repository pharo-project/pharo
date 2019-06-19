Extension { #name : #ClyFFIMethodQuery }

{ #category : #'*Calypso-SystemPlugins-FFI-Browser' }
ClyFFIMethodQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph definitionIcon: #recoverLostChanges
]

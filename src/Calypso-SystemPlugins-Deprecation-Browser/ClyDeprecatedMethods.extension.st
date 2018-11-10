Extension { #name : #ClyDeprecatedMethods }

{ #category : #'*Calypso-SystemPlugins-Deprecation-Browser' }
ClyDeprecatedMethods >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label clyAddEmphasis: TextEmphasis struckOut
]

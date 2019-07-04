Extension { #name : #ClyDeprecatedMethodsQuery }

{ #category : #'*Calypso-SystemPlugins-Deprecation-Browser' }
ClyDeprecatedMethodsQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	super decorateMethodGroupTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph label clyAddEmphasis: TextEmphasis struckOut
]

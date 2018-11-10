Extension { #name : #ClyLocalTraitsVisibilityLevel }

{ #category : #'*Calypso-SystemPlugins-Traits-Browser' }
ClyLocalTraitsVisibilityLevel >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	| tool |
	tool := ClyMethodVisibilityMorph for: anItemCellMorph browser level: self.
	tool build.
	anItemCellMorph addExtraIcon: #trait.
	anItemCellMorph addExtraTool: tool
]

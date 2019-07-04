Extension { #name : #ClySuperclassVisibilityLevel }

{ #category : #'*Calypso-SystemTools-Core' }
ClySuperclassVisibilityLevel >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	| tool |
	tool := ClyMethodVisibilityMorph for: anItemCellMorph browser level: self.
	tool build.
	anItemCellMorph definitionMorph: tool
]

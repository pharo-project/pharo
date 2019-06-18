Extension { #name : #ClyFullVisibilityLevel }

{ #category : #'*Calypso-SystemTools-Core' }
ClyFullVisibilityLevel >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	| tool |
	tool := ClyMethodVisibilityMorph for: anItemCellMorph browser level: self.
	tool build.
	anItemCellMorph addExtraIcon: #hierarchy.
	anItemCellMorph addExtraTool: tool
]

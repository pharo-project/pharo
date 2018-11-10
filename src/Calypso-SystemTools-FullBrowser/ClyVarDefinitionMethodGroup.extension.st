Extension { #name : #ClyVarDefinitionMethodGroup }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyVarDefinitionMethodGroup >> decorateTableCell: anItemCellMorph of: groupItem [
	| currentFont |
	super decorateTableCell: anItemCellMorph of: groupItem.
	
	currentFont := anItemCellMorph label font.
	anItemCellMorph label font: (LogicalFont familyName: currentFont familyName pointSize: currentFont pointSize - 1).
	
	anItemCellMorph label color: Color gray.
]

Extension { #name : #ClyAllMethodGroup }

{ #category : #'*Calypso-SystemTools-FullBrowser' }
ClyAllMethodGroup >> decorateTableCell: anItemCellMorph of: groupItem [
	super decorateTableCell: anItemCellMorph of: groupItem.
	
	anItemCellMorph browser metaLevelScope = ClyClassSideScope ifTrue: [ 
		anItemCellMorph label clyAddEmphasis: TextEmphasis bold]
]

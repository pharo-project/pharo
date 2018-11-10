Extension { #name : #ClyProjectChildItem }

{ #category : #'*Calypso-SystemTools-Core' }
ClyProjectChildItem >> decorateTableCell: anItemCellMorph inContext: itemContext [

	^ self
		decorateTableCell: anItemCellMorph
		of: itemContext lastSelectedItem
]

{ #category : #'*Calypso-SystemTools-Core' }
ClyProjectChildItem >> decorateTableCell: anItemCellMorph of: groupItem [
]

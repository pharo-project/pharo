Extension { #name : #ClyItemGroup }

{ #category : #'*Calypso-Browser' }
ClyItemGroup >> decorateTableCell: anItemCellMorph of: groupItem [

	self isEditableGroup ifFalse: [ 
		anItemCellMorph label color: anItemCellMorph theme calypsoFrozenItemColor]
	
]

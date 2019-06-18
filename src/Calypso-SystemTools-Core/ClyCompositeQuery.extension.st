Extension { #name : #ClyCompositeQuery }

{ #category : #'*Calypso-SystemTools-Core' }
ClyCompositeQuery >> decorateMethodGroupTableCell: anItemCellMorph of: groupItem [
	subqueries do: [:each |
		each decorateMethodGroupTableCell: anItemCellMorph of: groupItem  ]
]

{ #category : #'*Calypso-SystemTools-Core' }
ClyCompositeQuery >> decorateResultMethodEditor: aMethodEditor [

	subqueries do: [ :each | each decorateResultMethodEditor: aMethodEditor]
]

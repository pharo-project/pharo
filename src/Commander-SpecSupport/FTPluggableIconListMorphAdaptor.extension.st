Extension { #name : #FTPluggableIconListMorphAdaptor }

{ #category : #'*Commander-SpecSupport' }
FTPluggableIconListMorphAdaptor >> createCommandContextBy: aPresenter forDropAt: anEvent [

	| dropElementIndex receiverElement |
	dropElementIndex := self container rowIndexContainingPoint: anEvent position.
	dropElementIndex ifNil: [ ^super createCommandContextBy: aPresenter forDropAt: anEvent ].
	
	receiverElement := dataSource elementAt: dropElementIndex.
	^aPresenter createCommandContextForSelection: { receiverElement }
]

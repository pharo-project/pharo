Extension { #name : #SpAbstractTextPresenter }

{ #category : #'*NewTools-SpTextPresenterDecorators' }
SpAbstractTextPresenter >> addTextSegmentDecoration: aDecorationSegment [
	self adapter ifNotNil:[:a| a addTextSegmentDecoration: aDecorationSegment]
]

{ #category : #'*NewTools-SpTextPresenterDecorators' }
SpAbstractTextPresenter >> removeAllTextSegmentDecorations [
	self adapter ifNotNil:[:a| a removeAllTextSegmentDecorations]
]

Extension { #name : #SpEditableListPresenter }

{ #category : #'*Spec2-Examples' }
SpEditableListPresenter class >> example [
	<sampleInstance>
	| presenter |
	presenter := self new
		items: self selectors asOrderedCollection;
		addItemBlock: [ 1 ];
		openWithSpec;
		yourself.

	presenter withWindowDo: [ :w | w title: 'Selectors list' ].

	^ presenter
]

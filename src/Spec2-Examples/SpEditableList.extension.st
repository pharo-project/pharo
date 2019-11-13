Extension { #name : #SpEditableList }

{ #category : #'*Spec2-Examples' }
SpEditableList class >> example [
	<sampleInstance>
	| presenter |
	presenter := self new
		list: self selectors;
		openWithSpec;
		yourself.

	presenter withWindowDo: [ :w | w title: 'Selectors list' ].

	^ presenter
]

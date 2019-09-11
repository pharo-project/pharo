Extension { #name : #SpEditableList }

{ #category : #'*Spec2-Examples' }
SpEditableList class >> example [
	<sampleInstance>
	^self new
	
		title: 'Selectors list';
		list: self selectors;
	 openWithSpec
]

Extension { #name : #EditableList }

{ #category : #'*Spec-Examples' }
EditableList class >> example [
	<sampleInstance>
	^self new
	
		title: 'Selectors list';
		list: self selectors;
	 openWithSpec
]

Extension { #name : #LabelledList }

{ #category : #'*Spec-Examples' }
LabelledList class >> example [
	<sampleInstance>
	| example |
	example := self abstractExample.
	example
		items: {'item 1' . 'item 2'};
		openWithSpec.
	^ example
]

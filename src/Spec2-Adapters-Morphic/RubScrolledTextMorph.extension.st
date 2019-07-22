Extension { #name : #RubScrolledTextMorph }

{ #category : #'*Spec2-Adapters-Morphic' }
RubScrolledTextMorph >> registerScrollChanges: aSelector [
	self scrollPane announcer when: PaneScrolling send: aSelector to: self model
]

{ #category : #'*Spec2-Adapters-Morphic' }
RubScrolledTextMorph >> setSelectionSelector: aSelector [
	setSelectionSelector := aSelector
]

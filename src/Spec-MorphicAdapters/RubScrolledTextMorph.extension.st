Extension { #name : #RubScrolledTextMorph }

{ #category : #'*Spec-MorphicAdapters' }
RubScrolledTextMorph >> registerScrollChanges: aSelector [
	self scrollPane announcer when: PaneScrolling send: aSelector to: self model
]

{ #category : #'*Spec-MorphicAdapters' }
RubScrolledTextMorph >> setSelectionSelector: aSelector [
	setSelectionSelector := aSelector
]

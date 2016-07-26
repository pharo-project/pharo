selection change
selectedItems: aNodeItemCollection

	self listModel okToDiscardEdits ifFalse: [^ self].
	self emptySelection.
	(self nodeMorphsWithAllNodeItems: aNodeItemCollection) ifNotEmpty:[:selection |
	self addAllToSelection: selection].
	lastClickedMorph
		ifNil: [lastClickedMorph := self selectedMorphList
						ifEmpty: []
						ifNotEmpty: [self selectedMorphList last]]
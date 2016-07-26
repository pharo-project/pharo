as yet unclassified
resizeColumns
	| rowMorphsWidths |
	
	self columns size > 1 ifFalse: [ ^ self ].
	rowMorphsWidths := self rowMorphsWidths.
	self allNodeMorphs do: [ :node |
		node submorphs doWithIndex: [ :each :index | each hResizing: #rigid; width: (rowMorphsWidths at: index) ] ].
	self hHideScrollBar.
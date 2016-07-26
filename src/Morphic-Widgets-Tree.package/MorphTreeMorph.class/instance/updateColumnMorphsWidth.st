updating
updateColumnMorphsWidth
	| rowMorphsWidths |
	self columns size > 1 ifFalse: [^ self].
	rowMorphsWidths := self rowMorphsWidths.
	self allNodeMorphs do: [:i | i updateColumnMorphsWidthWith: rowMorphsWidths].

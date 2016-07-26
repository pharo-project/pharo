private
addMorphs
	"Add our morphs."

	| mainColumn |
	self preview: self newPreviewMorph.
	self taskList: self newTasksMorph.
	mainColumn := self theme newColumnIn: self for: {
		self preview.
		self taskList }.
	mainColumn
		vResizing: #shrinkWrap;
		cellInset: 8;
		cellPositioning: #center.
	self addMorph: mainColumn.
	self layoutChanged
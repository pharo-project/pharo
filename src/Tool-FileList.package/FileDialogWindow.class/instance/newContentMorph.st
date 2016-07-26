actions
newContentMorph
	"Answer a new content morph."

	self
		directoryTreeMorph: self newDirectoryTree;
		fileListMorph: self newFileList;
		previewMorph: self newPreviewMorph.
	^(self newRow: {
		self newColumn: {
			self newGroupbox: 'Directory' translated for: self directoryTreeMorph.
			(self newLabelGroup: {
				'File name' translated->self newFileNameTextEntry})
				vResizing: #shrinkWrap}.
		self newGroupbox: 'File' translated forAll: {
			self fileListMorph.
			self newActionButtonRow}},
		(self previewMorph notNil
			ifTrue: [{self newGroupbox: 'Preview' translated for: self previewMorph}]
			ifFalse: [#()]))
		vResizing: #spaceFill
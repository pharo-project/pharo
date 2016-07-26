public editing
openEditorWithContents: aText thenDo: aBlock
	self
		initialContents: aText;
		openEditor.
	aBlock cull: editor
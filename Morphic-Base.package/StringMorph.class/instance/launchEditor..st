private editing
launchEditor: evt
	" Launch an editor to edit my contents "
	self editorProvider ifNil: [ ^self ].
	self editorProvider openEditorForStringMorph: self
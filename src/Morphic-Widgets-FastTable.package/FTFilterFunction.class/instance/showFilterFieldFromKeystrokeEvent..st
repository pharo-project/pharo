private
showFilterFieldFromKeystrokeEvent: anEvent
	| text ed |
	text := anEvent keyCharacter asString asText.
	self initializeFilter.
	self filterWith: text.
	ed := RubFloatingEditorBuilder new
		customizeEditorWith: [ :editor | editor bounds: (self table bottomLeft + (0 @ 2) corner: self table bottomRight + (0 @ (editor font height + 6))) ];
		withEditedContentsDo: [ :contents :editor | 
			self filterWith: contents.
			editor setTextWith: (self colorText: contents) ].
	ed autoAccept: true.
	ed whenEditorEscapedDo:[self reinitializeTable ].
	ed openEditorWithContents: (self colorText: text)
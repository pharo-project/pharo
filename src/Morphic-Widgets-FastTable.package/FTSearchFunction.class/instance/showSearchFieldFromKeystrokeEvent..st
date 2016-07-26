private
showSearchFieldFromKeystrokeEvent: anEvent
	| s ed founds |
	s := anEvent keyCharacter asString asText.
	founds := self searchFor: s.
	ed := RubFloatingEditorBuilder new
		customizeEditorWith: [ :editor | editor bounds: (self table bottomLeft + (0 @ 2) corner: self table bottomRight + (0 @ (editor font height + 6))) ];
		withEditedContentsDo:
				[ :contents :editor | 
			contents
				ifEmpty: [ self table selectRowIndex: currentSelIndex. 
					self table highlightRowIndexes: currentHighlightedIndexes ]
				ifNotEmpty:
					[ founds := self searchFor: contents.
					founds
						ifTrue: [ editor
								setTextWith:
									(contents
										makeAllColor: self table theme textColor;
										yourself) ]
						ifFalse: [ editor
								setTextWith:
									(contents
										addAttribute: (TextColor new color: Color red);
										yourself) ] ] ];
		whenEditorEscapedDo: [ self table selectRowIndex: currentSelIndex.
			self table highlightRowIndexes: currentHighlightedIndexes ].
	ed autoAccept: true.
	founds
		ifFalse: [ s addAttribute: (TextColor new color: Color red) ]
		ifTrue: [ s makeAllColor: self table theme textColor ].
	ed openEditorWithContents: s
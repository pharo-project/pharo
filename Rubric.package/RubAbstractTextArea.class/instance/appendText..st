accessing text
appendText: aStringOrText
	self
		handleEdit:
			[ self
				beEditableWhile:
					[ self selectInvisiblyFrom: self text size + 1 to: self text size.
					self editor replaceSelectionWith: aStringOrText.
					self selectInvisiblyFrom: self text size + 1 to: self text size ] ]
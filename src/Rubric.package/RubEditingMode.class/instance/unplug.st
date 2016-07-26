initialize-release
unplug
	previousEditorClass = self textArea editor class
		ifFalse: [ 
			self textArea editorClass: previousEditorClass.
			self textArea releaseEditor ].
	self updateTextAreaWhenUnplugged.
	textArea := nil.

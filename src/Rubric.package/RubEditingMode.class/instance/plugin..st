initialize-release
plugin: aTextArea 
	textArea := aTextArea.
	previousEditorClass := self textArea editorClass.
	previousEditorClass = self editorClass
		ifFalse: [ 
			self textArea editorClass: self editorClass.
			self textArea releaseEditor].
	self updateTextAreaWhenPlugged
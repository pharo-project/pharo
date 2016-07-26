private
textInputFor: aTreeModel withHelpMessage: helpStringMessage
	| textInput |
	textInput := self theme
		newTextEntryIn: nil
		for: aTreeModel
		get: #inputText
		set: #inputText:
		class: String
		getEnabled: #inputTextEnabled
		help: helpStringMessage.
	textInput askBeforeDiscardingEdits: false.
	textInput ghostText: helpStringMessage.
	textInput layoutFrame: (LayoutFrame fractions: (0 @ 1 corner: 1 @ 1) offsets: (0 @ -26 corner: 0 @ -4)).
	^ textInput
actions
insertCompletion: aString

	| caret old pos editor offset|
	editor := controller editor.
	caret := editor caret.
	editor selectInvisiblyFrom: caret - context completionToken size to: caret - 1.
	old := editor selection. 
	editor zapSelectionWith: aString.
	offset := NECPreferences spaceAfterCompletion
				ifTrue: [ 1 ]
				ifFalse: [ 0 ].
	pos := caret + (aString copyUpTo: $ ) size + offset - old size.
	editor selectAt: pos.
	editor morph invalidRect: editor morph bounds
code panel
updateCodePanel
	sourceCodePanel removeAllMorphs.
	self model showComment
		ifTrue: [ self buildCodePanelWithComment ]
		ifFalse: [ self buildCodePanelWithoutComment ]
code panel
buildCodePanelWithoutComment
	sourceCodePanel removeAllMorphs.
	self commentTextModel announcer unsubscribe: self.
	commentTextModel := nil.
	sourceCodePanel addMorph: sourceTextWidget fullFrame: LayoutFrame identity
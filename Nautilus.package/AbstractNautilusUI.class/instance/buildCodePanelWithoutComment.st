code panel
buildCodePanelWithoutComment
	sourceCodePanel removeAllMorphs.
	self classCommentTextModel announcer unsubscribe: self.
	classCommentTextModel := nil.
	sourceCodePanel addMorph: sourceTextWidget fullFrame: LayoutFrame identity
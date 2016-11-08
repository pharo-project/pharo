comment pane
whenExtendCommentSwitch: anAnnouncement
	"when you press the comment button."
	self fullDisplayedComment
		ifFalse: [ self toggleShowFullComment ] 
		ifTrue: [ self buildCodePanelWithCommentOnRight ].
	self fullDisplayedComment: self fullDisplayedComment not.
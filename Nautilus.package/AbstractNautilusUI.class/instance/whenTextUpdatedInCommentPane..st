class comment pane
whenTextUpdatedInCommentPane: anAnnouncement
	self selectedClass ifNotNil:
		 [ :class | classCommentTextModel getText = class classCommentBlank ifFalse: [ class comment: classCommentTextModel getText stamp: Author changeStamp ] ]
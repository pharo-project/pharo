comment pane
whenTextUpdatedInCommentPane: anAnnouncement
	self selectedClass
		ifNotNil: [ :class | 
			commentTextModel getText = class classCommentBlank
				ifFalse: [ class comment: commentTextModel getText stamp: Author changeStamp ] ]
		ifNil: [ self packageCommentEmptyOrUnchanged ifTrue: [ ^ self ]
			ifFalse:[ 
			(self selectedPackage isNotNil and:[ self selectedPackage isGroup not])
				ifTrue: [ self updatePackageComment] ] ]
instance creation
newWith: aClassCommentedAnnouncement 

	^self
		newWithBehavior: aClassCommentedAnnouncement classCommented asEpiceaRingDefinition
		oldComment: aClassCommentedAnnouncement oldComment asString
		newComment: aClassCommentedAnnouncement newComment asString
		oldStamp: aClassCommentedAnnouncement oldStamp
		newStamp: aClassCommentedAnnouncement newStamp
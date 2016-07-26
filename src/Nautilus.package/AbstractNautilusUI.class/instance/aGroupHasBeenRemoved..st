group announcements
aGroupHasBeenRemoved: anAnnouncement
	 "(NautilusUI methodDict at: #aGroupHasBeenRegistered:) getSource"

	window ifNil: [ ^ self ].
	window isDisplayed ifFalse: [^ self ].
	(self groupsManager =  anAnnouncement holder)
		ifTrue: [ 
			self selectedGroup = anAnnouncement group
				ifTrue: [ self selectedGroup: nil ].
			self updateGroupView.
			self updateCategoryAndMethodList ]
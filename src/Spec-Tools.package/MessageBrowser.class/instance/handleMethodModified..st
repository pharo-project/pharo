announcements
handleMethodModified: anAnnouncement
	| item oldItem sel index text list edits |
	self isDisplayed
		ifFalse: [ ^ self ].
	refreshingBlockHolder ifNil: [ ^ self ].
	item := anAnnouncement newMethod.
	oldItem := anAnnouncement oldMethod.
	sel := listModel selectedItem.
	sel ifNil: [ ^ self ].
	(sel methodClass = oldItem methodClass
		and: [ sel selector = oldItem selector ])
		ifFalse: [ ^ self ].
	(self shouldRefreshItem: item fromAnnouncement: anAnnouncement)
		ifFalse: [ ^ self ].
	edits := textModel hasUnacceptedEdits.
	edits
		ifTrue: [ text := textModel pendingText ].
	index := listModel selectedIndex.
	list := listModel listItems
		remove: sel ifAbsent: [  ];
		add: item asFullRingDefinition;
		"to ensure it's still a RGMethod"
			yourself.
	self messages: list.
	listModel setSelectedIndex: index.
	edits
		ifFalse: [ ^ self ].
	textModel pendingText: text.
	textModel hasEditingConflicts: true
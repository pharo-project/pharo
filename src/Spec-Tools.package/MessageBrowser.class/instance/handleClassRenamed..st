announcements
handleClassRenamed: anAnnouncement
	
	| items selectedIndex |
	items := listModel listItems collect: [ :rgMethod | | interestedClassName interestedClass |
		interestedClassName := anAnnouncement oldName.
		interestedClass := anAnnouncement classRenamed.
		rgMethod isMetaSide ifTrue: [ 
			interestedClassName := interestedClassName, ' class'.
			interestedClass := interestedClass theMetaClass ].
		rgMethod parentName = interestedClassName ifTrue: [ 
			(interestedClass >> rgMethod selector) asFullRingDefinition.
		 	] ifFalse: [ rgMethod ] ].
	
	selectedIndex := listModel selectedIndex.
	self messages: items.
	listModel setSelectedIndex: selectedIndex.
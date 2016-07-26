rendering
actOnContextChanged: anAnnouncement 
	anAnnouncement property = #selection ifTrue: [ 
		treeModel announcer 
			suspendAll: self suspendedAnnouncements
			while: [
				treeMorph model explicitSelection: anAnnouncement value ] ] 
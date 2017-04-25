callbacks
actOnContextChanged: anAnnouncement 
	anAnnouncement property = #selection ifTrue: [ 
		tableModel announcer 
			suspendAll: self suspendedAnnouncements
			while: [ 
				tableModel explicitSelection: anAnnouncement value ] ]
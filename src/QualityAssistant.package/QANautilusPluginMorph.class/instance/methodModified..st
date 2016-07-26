announcement handling
methodModified: anAnnouncement 

	anAnnouncement method == entity ifTrue: [ 
		self updateList ]
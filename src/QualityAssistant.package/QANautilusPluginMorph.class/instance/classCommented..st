announcement handling
classCommented: anAnnouncement 

	anAnnouncement classCommented == entity ifTrue: [ 
		self updateList ]
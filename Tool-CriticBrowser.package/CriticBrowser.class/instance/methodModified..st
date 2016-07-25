system annoucements
methodModified: anAnnouncement
	"checks if a modification of a method fixes a warning. if true,  the true positives in the browser are update"
	"checks  if  a method of a manifest class is modified and update false positive in the browser"
		
	(criticModel criticsModelContains: anAnnouncement oldMethod)
		ifFalse: [ ^ self ].
	cache replaceAll: anAnnouncement oldMethod by: anAnnouncement newMethod.
	self reapplyThisRule	

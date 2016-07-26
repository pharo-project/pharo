system annoucements
classModified: anAnnouncement
	"checks if a modification of a class fixes a warning. if true,  the true/false positives in the browser are update"
	
	|  class  |
	class := anAnnouncement class.
	class isManifest ifTrue: [ ^ self ].	
	(criticModel criticsModelContains: anAnnouncement classAffected) ifFalse: [^ self].
	self reapplyThisRule	
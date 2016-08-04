private
addExclusion: anAnnouncement
	"Add anAnnouncement as an exclusion to me.
	I will explicitely not handle my exclusion announcements."
	
	^ exclusions add: anAnnouncement 
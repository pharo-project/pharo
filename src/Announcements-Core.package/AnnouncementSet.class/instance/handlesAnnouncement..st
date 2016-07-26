testing
handlesAnnouncement: anAnnouncement
	"If any of the set handles the announcements, subscribers should receive it."
	^ self anySatisfy: [ :each | each handlesAnnouncement: anAnnouncement ]
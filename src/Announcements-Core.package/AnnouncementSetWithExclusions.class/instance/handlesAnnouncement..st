testing
handlesAnnouncement: anAnnouncement
	"Return true when I will handle anAnnouncement.
	I extend my superclass behavior by explicitely 
	not handling the exclusions."

	^ (super handlesAnnouncement: anAnnouncement) 
			and: [ exclusions noneSatisfy: [ :each | each handlesAnnouncement: anAnnouncement ] ]
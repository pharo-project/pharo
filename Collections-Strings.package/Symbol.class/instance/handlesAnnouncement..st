announcements
handlesAnnouncement: anAnnouncement 
	"Anything else than the announcement identifier (in the case of symbol i.e. #foo for ... on: #foo send: #bar to: nil) will not 
	be handled."
	
	"Announcer new 
		on: #FOO send: #bar to: nil;
		announce: #FOO should raise DNU bar"
	
	^ anAnnouncement == self
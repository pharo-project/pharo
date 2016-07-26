announcing
jobAnnouncer
	"Answers the announcer for job announcements"
	
	^ jobAnnouncer 
			ifNil: [ jobAnnouncer := Announcer new ]
accessing
forbiddenAnnouncements
	"forbiddenAnnouncements holds a dictionary in which the key is a pane and the value is a collection of announcements"
	^ forbiddenAnnouncements ifNil: [forbiddenAnnouncements := IdentityDictionary new]
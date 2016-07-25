announcements
shouldRefreshItem: item fromAnnouncement: anAnnouncement
	
	"The refreshing block holder contains a block that accepts an item of the list, the received announcement and the message browser.
	It will anwer a boolean indicating if the modified item should be refreshed.
	
	Concretely, this is used to know if the contents of the selected message should be refreshed"
	^ refreshingBlockHolder value
		cull: item
		cull: anAnnouncement
		cull: self
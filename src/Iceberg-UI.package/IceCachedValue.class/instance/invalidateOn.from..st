initialization
invalidateOn: announcementType from: announcer
	announcer subscribe: announcementType send: #reset to: self
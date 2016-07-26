initialize
initialize
	"self initialize"
	registry ifNotNil: [ registry rehash ].
	self registerForNotifications.
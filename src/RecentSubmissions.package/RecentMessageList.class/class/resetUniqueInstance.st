instance creation
resetUniqueInstance
	"Reset the singleton of the recentMessageBrowser model, so if you execute it you will lose all the history of the recent messages."
	"self resetUniqueInstance"
	
	self stopNotifications.
	self uniqueInstance triggerEvent: #reset.
	UniqueInstance := nil
	
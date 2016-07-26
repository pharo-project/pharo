running
setUp 
	super setUp.
	oldSystemAnnouncer := SystemAnnouncer uniqueInstance.
	SystemAnnouncer restoreAllNotifications
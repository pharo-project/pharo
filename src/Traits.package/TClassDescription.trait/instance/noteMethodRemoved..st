organization updating
noteMethodRemoved: oldMethod
	self flag: #todo. "protocol and origin are not set properly here"
	SystemAnnouncer uniqueInstance methodRemoved: oldMethod protocol: oldMethod protocol origin: oldMethod origin
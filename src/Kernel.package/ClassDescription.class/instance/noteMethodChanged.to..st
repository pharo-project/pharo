organization updating
noteMethodChanged: oldMethod to: newMethod
	self flag: #todo. "oldProtocol is not set properly here"
	SystemAnnouncer uniqueInstance methodChangedFrom: oldMethod to: newMethod oldProtocol: Protocol unclassified
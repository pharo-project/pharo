private
stopNotification
	"(self instVarNamed: #default) stopNotification"
	
	"pay attention that we can break the system using this method"
	
	SystemAnnouncer uniqueInstance unsubscribe: self.
	MCWorkingCopy removeDependent: self.
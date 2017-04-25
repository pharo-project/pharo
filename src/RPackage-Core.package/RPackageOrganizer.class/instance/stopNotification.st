private
stopNotification
	"(self instVarNamed: #default) stopNotification"
	
	"pay attention that we can break the system using this method"
	
	SystemAnnouncer uniqueInstance unsubscribe: self.
	
	self class environment at: #MCWorkingCopy ifPresent: [:wc | 
		wc removeDependent: self].
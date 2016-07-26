updating
unregister
	"Unregister from the dependent object. Typically used when closing the browser"
	
	SystemAnnouncer uniqueInstance unsubscribe: self.
	MCWorkingCopy removeDependent: self.

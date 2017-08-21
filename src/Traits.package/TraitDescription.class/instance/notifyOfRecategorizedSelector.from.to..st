organization updating
notifyOfRecategorizedSelector: selector from: oldCategory to: newCategory
	"If compiled method is not there, it meens it has been removed, not recategorized... so I skip 
	 the method recategorized announce"
	(self compiledMethodAt: selector ifAbsent: [ nil ]) 
		ifNotNil: [ :method | 
			SystemAnnouncer uniqueInstance 
				methodRecategorized: method
				oldProtocol: oldCategory ].
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		self 
			notifyUsersOfRecategorizedSelector: selector 
			from: oldCategory 
			to: newCategory ]
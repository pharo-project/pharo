accessing method dictionary
removeSelector: selector 
	"Remove the message whose selector is given from the method 
	dictionary of the receiver, if it is there. Answer nil otherwise."
	
	| priorMethod priorProtocol origin | 
	priorMethod := self compiledMethodAt: selector ifAbsent: [^ nil].
	origin := priorMethod origin.
	priorProtocol := self whichCategoryIncludesSelector: selector.
	super removeSelector: selector.
	SystemAnnouncer uniqueInstance 
		suspendAllWhile: [self updateOrganizationSelector: selector oldCategory: priorProtocol newCategory: nil].
	SystemAnnouncer uniqueInstance methodRemoved: priorMethod protocol: priorProtocol origin: origin
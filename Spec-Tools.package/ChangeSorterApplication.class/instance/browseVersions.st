menu - message
browseVersions
	"Create and schedule a changelist browser on the versions of the 
	selected message."
	
	self selectedClass ifNotNil: [:class |
		self selectedSelector ifNotNil: [:selector || method |
			method := self createRingMethodForSelector: selector inClass: class.
			self model browseVersionsFrom: method ]]
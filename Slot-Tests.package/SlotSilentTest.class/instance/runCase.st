running
runCase

	SystemAnnouncer uniqueInstance suspendAllWhile: [ 
		Author useAuthor: self class name asString during: [ super runCase ]
	]

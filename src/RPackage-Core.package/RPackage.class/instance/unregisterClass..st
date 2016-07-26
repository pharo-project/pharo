private-register
unregisterClass: aClass
	"Private method that declares the mapping between a class and its package."
	self organizer
		unregisterPackage: self forClass: aClass.
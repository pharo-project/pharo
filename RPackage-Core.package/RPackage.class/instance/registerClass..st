private-register
registerClass: aClass
	"Private method that declares the mapping between a class and its package."
	self organizer
		registerPackage: self forClass: aClass
	
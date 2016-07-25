private-register
registerClassName: aClassNameSymbol
	"Private method that declares the mapping between a class and its package."
	self organizer
		registerPackage: self forClassName: aClassNameSymbol
	
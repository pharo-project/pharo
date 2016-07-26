initialization
initialize

	super initialize.
	self withoutPackages.	"By default the receiver has no child packages"
	self extensionMethods: IdentityDictionary new.		
	self categories: OrderedCollection new.
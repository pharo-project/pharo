class initialization
initialize
	"self initialize"
	
	"Ensure the organizer will be the RPackageOrganizer default"
	self organizer: nil.
	Properties
		ifNil: [ self initializeProperties ]
		ifNotNil: [ | newDict | 
			newDict := WeakIdentityKeyDictionary newFrom: Properties.
			newDict rehash.
			Properties := newDict ]
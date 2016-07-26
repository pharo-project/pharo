private
setDefault: anOrganizer 
	default ifNotNil: [ default unregister ].
	default := anOrganizer.
	default register.
	RPackage organizer: nil.
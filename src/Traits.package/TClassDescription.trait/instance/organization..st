organization
organization: aClassOrg
	"Install an instance of ClassOrganizer that represents the organization of the messages of the receiver."

	aClassOrg ifNotNil: [aClassOrg setSubject: self].
	self basicOrganization: aClassOrg.
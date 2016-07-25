organization
organization
	"Answer the instance of ClassOrganizer that represents the organization 
	of the messages of the receiver."

	organization ifNil: [
		self organization: (ClassOrganization forClass: self) ].
	"Making sure that subject is set correctly. It should not be necessary."
	organization ifNotNil: [ organization setSubject: self ].
	^ organization
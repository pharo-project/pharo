accessing
organization: anOrganization
	"Return the organizer for the receiver"
	
	anOrganization environment: self.
	^self at: #SystemOrganization put: anOrganization
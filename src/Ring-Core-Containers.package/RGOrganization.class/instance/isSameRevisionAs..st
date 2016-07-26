testing
isSameRevisionAs: aRGOrganization
	"This method look for equality of the properties of the receiver"

	^(self class = aRGOrganization class)
		and:[ self categories = aRGOrganization categories ]
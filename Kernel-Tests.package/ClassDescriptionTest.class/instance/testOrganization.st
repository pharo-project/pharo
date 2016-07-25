tests
testOrganization

	| aClassOrganizer |
	aClassOrganizer := ClassDescription organization.
	self assert: (aClassOrganizer isKindOf: ClassOrganization).
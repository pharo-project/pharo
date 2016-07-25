testing
testConvertingMCOrganizationDefinition
	| mcOrganization ringOrganization mcOrganization2 |
	
	mcOrganization := MCOrganizationDefinition categories: {#'Ring-Core-Kernel' . #'Ring-Core-Definitions'}.
	mcOrganization2 := MCOrganizationDefinition categories: {#'Ring-Core-Kernel' . #'Ring-Core-Definitions' . #'Ring-Core-Variables'}.
	ringOrganization := mcOrganization asRingDefinition.
	
	self assert: (ringOrganization isOrganization).
	self deny: (ringOrganization isSameRevisionAs: mcOrganization2 asRingDefinition)
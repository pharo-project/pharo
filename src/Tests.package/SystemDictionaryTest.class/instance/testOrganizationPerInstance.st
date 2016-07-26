tests
testOrganizationPerInstance
	"self debug: #testOrganizationPerInstance"
	
	| aDictionary otherDictionary |	
	aDictionary := SystemDictionary withOrganizer: SystemOrganizer new.
	otherDictionary := SystemDictionary withOrganizer: SystemOrganizer new.
	self deny: aDictionary organization = otherDictionary organization.
	
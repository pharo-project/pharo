running
setUp
	| environment |
	super setUp.
	environment := SystemDictionary withOrganizer: SystemOrganizer new.
	factory := ClassFactoryWithOrganization newWithOrganization: environment organization.
	
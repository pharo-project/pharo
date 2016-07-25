tests
testLoadAndUnload
	| category |
	category := 'TestPackageToUnload'.
	SystemOrganization addCategory: category.
	(MCOrganizationDefinition categories: { category }) unload.
	self deny: (SystemOrganization includesCategory: category)
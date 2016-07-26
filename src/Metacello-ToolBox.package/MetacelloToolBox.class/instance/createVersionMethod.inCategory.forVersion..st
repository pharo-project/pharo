api-configuration
createVersionMethod: selector inCategory: category forVersion: versionString
	
	methodSpec := (MetacelloVersionMethodSpec new)
		project: project;
		selector: selector;
		category: category;
		versionString: versionString;
		yourself.
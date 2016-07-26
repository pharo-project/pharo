parsing
extractDependencyFrom: zipMember
	^ MCVersionDependency
		package: (MCPackage named: (zipMember fileName copyAfterLast: $/))
		info: (self extractInfoFrom: (self parseMember: zipMember))
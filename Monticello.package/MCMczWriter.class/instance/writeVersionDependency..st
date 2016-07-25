visiting
writeVersionDependency: aVersionDependency
	| string |
	string := (self serializeVersionInfo: aVersionDependency versionInfo).
	self addString: string at: 'dependencies/', aVersionDependency package name encodedTo: 'utf8'
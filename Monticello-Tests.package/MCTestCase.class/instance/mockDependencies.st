mocks
mockDependencies
	^ Array with: (MCVersionDependency package: self mockEmptyPackage info: (self mockVersionInfo: 'x'))
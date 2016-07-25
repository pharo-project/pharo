converting
dependencyFromArray: anArray
	^MCVersionDependency
		package: (MCPackage named: anArray first)
		info: (
			MCVersionInfo
			name: anArray second
			id: (UUID fromString: anArray third)
			message: nil
			date: nil
			time: nil
			author: nil
			ancestors: nil)
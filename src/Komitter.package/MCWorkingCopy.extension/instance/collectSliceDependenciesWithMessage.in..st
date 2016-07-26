*Komitter
collectSliceDependenciesWithMessage: messageString in: aRepository
	
	^ self requiredPackages collect: [:aPackage | 
			MCVersionDependency
				package: aPackage
				info:  aPackage version info ]
	
operations
collectDependenciesWithMessage: messageString in: aRepository
	
	^ self requiredPackages collect: [:aPackage | 
			MCVersionDependency
				package: aPackage
				info:  (aPackage workingCopy 
					currentVersionInfoWithMessage: messageString 
					in: aRepository) ]
	
utilities
loadDependencies
	"Should change that code by the following: force the reading of dependencies the hard way."

	"Here, in fact, what we need to do is to try to change the dependency info because we won't be directly in charge of loading those?"

	| dependencyDir singleGitCommit isSingleCommit |
	dependencyDir := packageDirectory , self slash , self monticelloMetaDirName , self slash , 'dependencies' , self slash.
	singleGitCommit := packageDirectory , self slash , self monticelloMetaDirName , self slash
		, MCFileTreeGitRepository gitPerPackageProperties.
	isSingleCommit := self zip members anySatisfy: [ :entry | entry fileName = singleGitCommit ].
	dependencies := OrderedCollection new.
	MCCacheRepository uniqueInstance
		cacheAllFileNamesDuring: [ 
			repository
				cacheAllFileNamesDuring: [ 
					(self zip members select: [ :entry | entry isDirectory not and: [ entry fileName beginsWith: dependencyDir ] ])
						do: [ :entry | 
							| name |
							name := entry splitFileName last.
							dependencies
								add:
									(isSingleCommit
										ifTrue: [ MCResolvedVersionDependency package: (MCPackage named: name) fromGit: commitID in: repository ]
										ifFalse: [ 
											MCVersionDependency
												package: (MCPackage named: name)
												info: (self extractInfoFrom: (self parseMember: 'dependencies' , self slash , name)) ]) ] ] ].
	dependencies := dependencies asArray
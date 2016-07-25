menu spec
repositoryMenuSpec
	^ 	#(('Open repository' #openRepository)
		   ('Edit repository info' #editRepository)
		   ('Add to package...' #addRepositoryToPackage)
		   ('Remove repository' #removeRepository)	
		   ('Load repositories' #loadRepositories)	
		   ('Save repositories' #saveRepositories)
		   ('Flush cached versions' #flushAllCaches))
		
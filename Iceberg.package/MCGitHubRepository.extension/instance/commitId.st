*Iceberg-Core
commitId
	"This kind of repo is associated to a specific commit that was downloaded, 
	sadly the only way to know it is to parse the directory name."
	^ self directory basename copyAfterLast: $-.
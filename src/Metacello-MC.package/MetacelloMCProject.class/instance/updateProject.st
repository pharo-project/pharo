development support
updateProject
	"load the latest version of the configuration package"

	"WARNING: don't forget to refresh your project instance after doing an #updateProject, 
	 otherwise your instance won't reflect the info in the freshly loaded configuration"
	
	^self updateProject: MetacelloLoaderPolicy new
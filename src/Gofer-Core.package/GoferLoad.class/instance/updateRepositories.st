private
updateRepositories
	"This code makes sure that all packages have a repository assigned, including the dependencies."

	self versions do: [ :version |
		gofer repositories do: [ :repository | 
			version workingCopy repositoryGroup
				addRepository: repository ] ]
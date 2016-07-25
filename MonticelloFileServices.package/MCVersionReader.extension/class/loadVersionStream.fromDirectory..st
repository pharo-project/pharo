*MonticelloFileServices
loadVersionStream: stream fromDirectory: directory
	| version repository |
	repository := MCDirectoryRepository new directory: directory.
	version := self versionFromStream: stream.
	version workingCopy repositoryGroup addRepository: repository.
	version load.

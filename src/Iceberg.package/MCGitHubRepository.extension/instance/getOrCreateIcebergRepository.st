*Iceberg-Adapters
getOrCreateIcebergRepository
	| remoteUrl remote |

	remoteUrl := self perform: Iceberg remoteTypeSelector.
	remote := IceRemote url: remoteUrl.
	^ IceRepository registry 
		detect: [ :repo | repo origin referencesSameRemoteLocationAs: remote ]
		ifNone: [
			(IceRepositoryCreator new 
				url: remoteUrl;
				subdirectory: repoPath;
				branchName: self projectVersion;
				createRepository)
				register ]
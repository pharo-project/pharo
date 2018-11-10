Extension { #name : #MCGitBasedNetworkRepository }

{ #category : #'*Iceberg' }
MCGitBasedNetworkRepository >> createIcebergRepositoryFor: remoteUrl [
	| builder repository |
	builder := IceRepositoryCreator new 
		url: remoteUrl;
		subdirectory: repoPath;
		commitishName: self projectVersion;
		ensureProjectFile;
		yourself.

	[ repository := builder createRepository ]
	on: IceDuplicatedRepository 
	do: [ :e | 
		e resignalAs: (IceMetacelloDuplicatedNotification new
			builder: builder;
			yourself)  ].
	
	repository register.
	
	^ repository
]

{ #category : #'*Iceberg' }
MCGitBasedNetworkRepository >> createIcebergRepositoryWithFallbackFor: remote url: remoteUrl [
	| urlToUse  |
	
	urlToUse := remoteUrl.
	[ ^ self createIcebergRepositoryFor: urlToUse ]
	on: IceAuthenticationError do: [ :e |
		self crLog: ('I got an error while cloning: {1}. I will try to clone the HTTPS variant.' format: { e messageText }). 
		urlToUse := remote httpsUrl.
		e retry ]
]

{ #category : #'*Iceberg' }
MCGitBasedNetworkRepository >> getOrCreateIcebergRepository [
	| remoteUrl |
	remoteUrl := self perform: Iceberg remoteTypeSelector.
	^ IceRepository registry 
		detect: [ :repo | repo isCloneOfUrl: remoteUrl ]
		ifNone: [ | remote |
			remote := IceGitRemote url: remoteUrl.
			self createIcebergRepositoryWithFallbackFor: remote url: remoteUrl ]
]

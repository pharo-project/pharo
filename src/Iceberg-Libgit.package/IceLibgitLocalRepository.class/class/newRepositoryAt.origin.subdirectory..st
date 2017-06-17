instance creation
newRepositoryAt: aFileReference origin: anIceRemoteRepository subdirectory: aString
	^ self new
		location: aFileReference;
		origin: anIceRemoteRepository;
		subdirectory: aString;
		yourself
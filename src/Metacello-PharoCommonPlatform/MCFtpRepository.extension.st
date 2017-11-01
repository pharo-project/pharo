Extension { #name : #MCFtpRepository }

{ #category : #'*metacello-pharocommonplatform' }
MCFtpRepository >> asRepositorySpecFor: aMetacelloMCProject [
	| dir |
	dir := directory.
	(directory at: 1) = $/
		ifFalse: [ dir := '/', dir ].
	^(aMetacelloMCProject repositorySpec)
		description:  'ftp://', host, dir;
	 	type: 'ftp';
		username: user;
		password: password;
		yourself
]

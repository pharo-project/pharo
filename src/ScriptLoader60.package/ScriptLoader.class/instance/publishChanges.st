menu integration actions
publishChanges
	| username password str strings|
	(FileStream isAFileNamed: 'PharoPass.txt') 
		ifTrue: [ str := FileStream fileNamed:  'PharoPass.txt'. 
				strings := str contents substrings.
				username := strings first.
				password := strings second] 
		ifFalse: [ username := UIManager default request: 'Pharo repository login'.
			     password := UIManager default requestPassword: 'Pharo repository password'.].

	self setToRepositoriesPassword: password to: username.
	
	self csForLastUpdateAndPatchUpdatesList: 'Update'.
	self copyPackagesFromWaitingFolderToHomeRepository.
	self announceOnMailingList.
	self inform: 'All packages have been uploaded to the Pharo repository.
Remaining manual steps:
1) ./pushUpdate 
2) Announce new update on mailing list'
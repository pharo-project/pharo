execute
execute
	| message repo gofer |
	message := self uiManager multilineRequest: 'Enter commit message' initialAnswer: ''.
	message == nil
		ifTrue: [ ^ self ].
	repo := self checkUserInRepositoryOfPackage: target spec name.
	gofer := Gofer new.
	gofer disablePackageCache.
	gofer repository: repo.
	gofer package: target spec name.
	gofer commit: message
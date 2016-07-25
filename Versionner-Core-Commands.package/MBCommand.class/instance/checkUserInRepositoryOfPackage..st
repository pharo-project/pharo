repositories
checkUserInRepositoryOfPackage: packageName
	"Try to find the http repository of packageName. If it does not find one, then add one and ask for the username and password"

	| repo |
	(repo := self repositoryOfPackageNamed: packageName) ifNil: [ ^nil ].
	self checkUserInRepository: repo.
	^repo
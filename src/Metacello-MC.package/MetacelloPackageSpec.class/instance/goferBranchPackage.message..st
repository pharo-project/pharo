development support
goferBranchPackage: branchName message: commitMessage
	"uses gofer to do commit ... non-interactive"

	| latestFile pkgSpec   |
	(file notNil and: [ (self name, '.', branchName) = self file ])
		ifTrue: [ 
			latestFile := self loader latestPackage: self file fromRepository: self repositorySpecs.
			pkgSpec := self copy.
			latestFile ~~ nil
				ifTrue: [ 
					pkgSpec file: latestFile.
					pkgSpec goferCommitPackage: commitMessage.
					^ self ] ].
	self loader goferCommitBranchPackage: branchName using: self repositorySpecs commitMessage: commitMessage
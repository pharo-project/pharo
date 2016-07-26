development support
goferCommitPackage: commitMessage
	"uses gofer to do commit ... non-interactive"
	
	| latestFile pkgSpec |
	^(file notNil and: [ self name = self file ])
		ifTrue: [
			latestFile := self loader latestPackage: self name fromRepository: self repositorySpecs.
			pkgSpec := self copy.
			pkgSpec file: latestFile.
			pkgSpec goferCommitPackage: commitMessage ]
		ifFalse: [ self loader goferCommitPackageUsing: self repositorySpecs commitMessage: commitMessage ]
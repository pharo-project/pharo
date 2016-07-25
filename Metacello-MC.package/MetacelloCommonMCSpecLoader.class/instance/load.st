actions
load
	| repos |
	repos := self repositoriesFrom: self spec repositorySpecs.
	^ self loadType == #atomic
		ifTrue: [self atomicLoadPackageSpecs: self spec packageSpecsInLoadOrder repositories: repos]
		ifFalse: ["assume #linear"
			self linearLoadPackageSpecs: self spec packageSpecsInLoadOrder repositories: repos ]
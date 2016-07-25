development support
updateProject: aLoaderPolicy
	"load the latest version of the configuration package"

	"WARNING: don't forget to refresh your project instance after doing an #updateProject, 
	 otherwise your instance won't reflect the info in the freshly loaded configuration"

	| mcLoader |
	(mcLoader := self loader) == nil
		ifTrue: [ mcLoader := self project loaderClass on: nil ].
	mcLoader loaderPolicy: aLoaderPolicy.
	mcLoader
		doingLoads: [ 
			MCWorkingCopy
				managersForClass: self configuration class
				do: [ :mgr | 
					| pkg ar |
					pkg := self packageSpec.
					mgr repositoryGroup repositories do: [ :repo | pkg repositories repository: (repo asRepositorySpecFor: self) ].
					ar := mgr metacelloPackageNameWithBranch.
					pkg name: (ar at: 1).
					(ar at: 2) notEmpty
						ifTrue: [ pkg file: (ar at: 2) ].
					pkg load.
					^ true ] ].
	^ true
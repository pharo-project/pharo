development support
fetchProject: aLoaderPolicy
	"fetch the latest version of the configuration package"
	
	| mcLoader |
	(mcLoader := self loader) == nil
		ifTrue: [ mcLoader := self project loaderClass on: nil ].
	mcLoader loaderPolicy: aLoaderPolicy.
	mcLoader doingLoads: [ 
		MCWorkingCopy
			managersForClass: self configuration class
			do: [:mgr | | pkg |
				pkg := self packageSpec.
				mgr repositoryGroup repositories do: [:repo | pkg repositories repository: (repo asRepositorySpecFor: self) ].
				pkg name: mgr packageName.
				pkg fetchUsing: mcLoader.
				^true ]].
	^true
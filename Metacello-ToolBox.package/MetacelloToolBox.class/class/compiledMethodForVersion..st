tool support
compiledMethodForVersion: aMetacelloVersion

	| toolbox pragma |
	toolbox := MetacelloToolBox new project: aMetacelloVersion project.
	pragma := (toolbox constructor extractAllVersionPragmas at: aMetacelloVersion versionString ifAbsent: [ ^ nil ]) first.
	^pragma method
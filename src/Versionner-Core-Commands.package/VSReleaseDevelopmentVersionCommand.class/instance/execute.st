execute
execute
	|  devVersion releaseDialog |

	[ devVersion := workflow developmentVersion ]
		on: MetacelloVersionDoesNotExistError
		do: [ UIManager default inform: 'Cannot find a development version for this project!'.
			^ self ].
	self ensureDevelopmentVersionIsBaseline.
	(releaseDialog := VersionnerReleasePanel openForVersion: devVersion)
		okAction: [ 
			self 
				release:  releaseDialog model releaseNumberText
				andSolveSymbolic: releaseDialog model isSymbolicVersionResolutionChecked ]
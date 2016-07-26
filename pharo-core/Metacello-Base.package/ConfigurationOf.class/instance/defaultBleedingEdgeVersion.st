defaults
defaultBleedingEdgeVersion
	| bleedingEdgeVersion |
	bleedingEdgeVersion := (self project map values select: [ :version | version blessing == #baseline ])
		detectMax: [ :version | version ].
	bleedingEdgeVersion ifNil: [ ^#'notDefined' ].
	^ bleedingEdgeVersion versionString
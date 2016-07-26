private
latestVersionString
	| latestVersion |
	
	latestVersion := (developmentVersion project versions 
		reject: [ :aVersion |  #(baseline broken) includes: aVersion blessing ])
		detectMax: [ :aVersion | aVersion ].
	^ latestVersion 
		ifNotNil:  [ latestVersion versionString ]
		ifNil: [ nil ] 
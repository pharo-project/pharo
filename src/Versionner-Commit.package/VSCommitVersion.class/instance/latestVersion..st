private
latestVersion: metacelloProject
	| latestVersion |
	
	latestVersion := (metacelloProject versions 
		reject: [ :each |  #(baseline broken) includes: each blessing ])
		detectMax: [ :each | each ].
	^ latestVersion 
		ifNotNil: [ latestVersion versionString ]
		ifNil: [ nil ] 
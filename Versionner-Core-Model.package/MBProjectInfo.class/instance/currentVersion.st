accessing
currentVersion
	currentVersion == nil
		ifTrue: [ 
			| projectClass version info |
			(projectClass := self spec projectClass) == nil
				ifTrue: [ 
					currentVersion := ''.
					^ currentVersion ].
			(info := self configurationRoot configurationInfoFor: projectClass) == nil
				ifTrue: [ 
					currentVersion := ''.
					^ currentVersion ].
			(currentVersion := info currentVersion) == nil
				ifTrue: [ 
					currentVersion := ''.
					^ currentVersion ].
			currentVersion := currentVersion versionString ].
	^ currentVersion
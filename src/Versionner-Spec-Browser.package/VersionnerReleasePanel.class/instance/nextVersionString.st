private
nextVersionString
	^ self latestVersionString 
		ifNotNil:  [ :versionString | | lastPointIndex |
					lastPointIndex := versionString 
						lastIndexOf: $. 
						ifAbsent: [ nil ].
					lastPointIndex 
						ifNil: [ (versionString asInteger + 1) asString ]
						ifNotNil: [ | newMinorVersion |
								newMinorVersion := (versionString allButFirst: lastPointIndex) asInteger + 1.
								(versionString  copyFrom: 1 to: lastPointIndex) , (newMinorVersion asString) ] ]
		ifNil: [ '1.0' ] 
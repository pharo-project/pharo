private
nextDevelopmentVersionString: aDevelopmentVersionString
	| versionNumber lastPointIndex newVersionNumber |
	versionNumber := self developmentVersionNumber: aDevelopmentVersionString.
	lastPointIndex := versionNumber 
						lastIndexOf: $. 
						ifAbsent: [ nil ].
	newVersionNumber := lastPointIndex 
		ifNil: [ versionNumber + 1 ]
		ifNotNil: [ | newMinorVersion |
				newMinorVersion := (versionNumber allButFirst: lastPointIndex) asInteger + 1.
				(versionNumber  copyFrom: 1 to: lastPointIndex) , (newMinorVersion asString) ].
	^ self standardizeDevVersionString: newVersionNumber

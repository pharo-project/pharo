execute
executeWithMessage: message
	| toolbox updatedSpecs |
	toolbox := MetacelloToolBox configurationNamed: target configurationBasename.
	toolbox
		updateVersionMethodForVersion: target version versionString
		updateProjects: false
		updatePackages: false
		versionSpecsDo: [ :attribute :versionSpec | 
			attribute == #common
				ifTrue: [ 
					"update the author, description and timeStamp"
					versionSpec
						author: MetacelloPlatform current authorName;
						timestamp: MetacelloPlatform current timestamp.
					MetacelloToolBox appendDescription: message to: versionSpec ].
			true ].
	toolbox commitMethod

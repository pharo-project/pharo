scripts
updateDevelopment: developmentVersionString for: configurationBasename updatePackages: updatePackages description: message
	"Update project specs and (conditionally) the package specs in the given version to reflect the new mcz file and project versions."

	<apiDocumentation>
	| toolbox updatedSpecs |
	toolbox := self configurationNamed: configurationBasename.
	updatedSpecs := toolbox
		updateVersionMethodForVersion: developmentVersionString
		updateProjects: true
		updatePackages: updatePackages
		versionSpecsDo: [ :attribute :versionSpec | 
			attribute == #common
				ifTrue: [ 
					"update the author, description and timeStamp"
					versionSpec
						author: MetacelloPlatform current authorName;
						timestamp: MetacelloPlatform current timestamp.
					self appendDescription: message to: versionSpec ].
			true ].
	updatedSpecs notEmpty
		ifTrue: [ toolbox commitMethod ].
	^ updatedSpecs
scripts
updateDescription: developmentVersionString for: configurationBasename description: message
	"Update description"

	<apiDocumentation>
	^ (self configurationNamed: configurationBasename)
		updateVersionMethodForVersion: developmentVersionString
			updateProjects: false
			updatePackages: false
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
						"update the author, description and timeStamp"
						versionSpec
							author: MetacelloPlatform current authorName;
							timestamp: MetacelloPlatform current timestamp.
						self appendDescription: message to: versionSpec ].
				true ];
		commitMethod
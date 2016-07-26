scripts
createDevelopment: developmentVersionString for: configurationBasename from: existingDevelopmentVersionString description: aString
	"Create a new development version based on the specifications in an existing development version. Modify the #development symbolic version to reference the new version."

	<apiDocumentation>
	| toolbox |
	(toolbox := self configurationNamed: configurationBasename)
		createVersionMethod: 'version' , (self stripVersionStringForSelector: developmentVersionString) , ':'
			inCategory: 'versions'
			forVersion: developmentVersionString;
		addSectionsFrom: existingDevelopmentVersionString
			forBaseline: false
			updateProjects: true
			updatePackages: true
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
						"update the author, blessing, description and timeStamp"
						versionSpec
							author: MetacelloPlatform current authorName;
							description: aString;
							blessing: #development;
							timestamp: MetacelloPlatform current timestamp ].
				true ];
		commitMethod.
	toolbox
		symbolicVersionMethod: #development;
		addSymbolicSection: #common version: developmentVersionString;
		commitMethod.
	^ toolbox
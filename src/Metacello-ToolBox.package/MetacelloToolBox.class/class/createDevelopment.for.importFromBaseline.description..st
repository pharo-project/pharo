scripts
createDevelopment: developmentVersionString for: configurationBasename importFromBaseline: baselineVersionString description: aString
	"Create a new development version based on the specifications in an existing baseline version. Modify the #development symbolic version to reference the new version."

	<apiDocumentation>
	| toolbox |
	(toolbox := self configurationNamed: configurationBasename)
		createVersionMethod: 'version' , (self stripVersionStringForSelector: developmentVersionString) , ':'
			inCategory: 'versions'
			forVersion: developmentVersionString;
		importFrom: baselineVersionString
			updateProjects: true
			updatePackages: true
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
						"update the author, blessing, description and timeStamp"
						versionSpec
							author: MetacelloPlatform current authorName;
							blessing: #development;
							description: aString;
							timestamp: MetacelloPlatform current timestamp ].
				true ];
		commitMethod.
	toolbox
		symbolicVersionMethod: #development;
		addSymbolicSection: #common version: developmentVersionString;
		commitMethod.
	^ toolbox
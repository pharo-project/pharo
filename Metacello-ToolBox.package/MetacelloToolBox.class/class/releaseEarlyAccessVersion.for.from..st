scripts
releaseEarlyAccessVersion: earlyAccessVersionString for: configurationBasename from: developmentVersionString
	"Update the #development and #earlyAccess symbolic versions."

	<apiDocumentation>
	| toolbox message previousEarlyAccessVersion previousEarlyAccessSeparator index earlyAccessDescription |
	self flag: 'Make this more general purpose'.
	toolbox := self configurationNamed: configurationBasename.
	message := (toolbox project version: developmentVersionString) description.
	previousEarlyAccessVersion := (MetacelloVersionNumber fromString: earlyAccessVersionString) decrementMinorVersionNumber
		asString.
	previousEarlyAccessSeparator := '---' , previousEarlyAccessVersion , '---'.
	(index := message indexOfSubCollection: previousEarlyAccessSeparator) > 0
		ifTrue: [ 
			index + previousEarlyAccessSeparator size + 1 <= message size
				ifTrue: [ earlyAccessDescription := message copyFrom: index + previousEarlyAccessSeparator size + 1 to: message size ] ].
	toolbox
		createVersionMethod: 'version' , (self stripVersionStringForSelector: earlyAccessVersionString) , ':'
			inCategory: 'versions'
			forVersion: earlyAccessVersionString;
		addSectionsFrom: developmentVersionString
			forBaseline: false
			updateProjects: true
			updatePackages: true
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
						"update the author, blessing, description and timeStamp"
						versionSpec
							author: MetacelloPlatform current authorName;
							description: earlyAccessDescription;
							blessing: #development;
							timestamp: MetacelloPlatform current timestamp ].
				true ];
		commitMethod.
	toolbox
		symbolicVersionMethod: #earlyAccess;
		addSymbolicSection: #common version: earlyAccessVersionString;
		commitMethod.
	toolbox
		modifyVersionMethodForVersion: developmentVersionString
			versionSpecsDo: [ :attribute :versionSpec | 
					attribute == #common
						ifTrue: [ 
							"update the author, description and timeStamp"
							versionSpec
								author: MetacelloPlatform current authorName;
								timestamp: MetacelloPlatform current timestamp.
							self appendDescription: '---' , earlyAccessVersionString , '---' to: versionSpec ].
					true ];
		commitMethod.
	^ toolbox
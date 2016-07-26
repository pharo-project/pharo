scripts
releaseVersion: developmentVersionString for: configurationBasename blessingAttribute: attribute platformAttribute: platformAttribute
	<apiDocumentation>
	"Set the blessing to #release and set the #stable symbolic version to the given version, using the given attributes for each."
	
	| toolbox |
	(toolbox := self configurationNamed: configurationBasename)
		modifyVersionMethodForVersion: developmentVersionString
			versionSpecsDo: [ :attr :versionSpec | 
					attr == attribute
						ifTrue: [ versionSpec blessing: #release ].
					true ];
		commitMethod;
		symbolicVersionMethod: #development;
		addSymbolicSection: platformAttribute version: #notDefined;
		commitMethod;
		symbolicVersionMethod: #stable;
		addSymbolicSection: platformAttribute version: developmentVersionString;
		commitMethod.
	^ toolbox
scripts
createBaseline: baselineVersionString for: configurationBasename from: existingBaselineVersionString description: aString versionSpecsDo: versionSpecBlock
	"Create a new baseline version based on the specification in an existing baseline. Evaluate <versionSpecBlock> for each section."

	<apiDocumentation>
	| toolbox |
	(toolbox := self configurationNamed: configurationBasename)
		createVersionMethod: 'baseline' , (self stripVersionStringForSelector: baselineVersionString) , ':'
			inCategory: 'baselines'
			forVersion: baselineVersionString;
		addSectionsFrom: existingBaselineVersionString
			forBaseline: true
			updateProjects: false
			updatePackages: false
			versionSpecsDo: [ :attribute :versionSpec | 
				attribute == #common
					ifTrue: [ 
						aString notEmpty
							ifTrue: [ versionSpec description: aString ] ].
				versionSpecBlock value: attribute value: versionSpec ];
		commitMethod.
	^ toolbox
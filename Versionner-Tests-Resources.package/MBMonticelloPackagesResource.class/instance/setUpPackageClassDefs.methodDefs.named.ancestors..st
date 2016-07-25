running
setUpPackageClassDefs: classDefArray methodDefs: methodDefArray named: packageName ancestors: ancestors
	"self reset"
	
	| definitions superclassName className ivNames cvNames civNames versionInfo methodSelector methodSource basePackageName authorName |
	definitions := OrderedCollection new.
	basePackageName := packageName copyUpToLast: $-.
	authorName := (packageName copyAfterLast: $-) copyUpTo: $..
	1 to: classDefArray size by: 5 do: [:index |
		superclassName := (classDefArray at: index) asSymbol.
		className := (classDefArray at: index + 1) asSymbol.
		ivNames := (classDefArray at: index + 2).
		cvNames := (classDefArray at: index + 3).
		civNames := (classDefArray at: index + 4).
		
		definitions add: (MCOrganizationDefinition categories: (Array with: basePackageName)).
		definitions add: (MCClassDefinition 
			name: className 
			superclassName: superclassName 
			category: basePackageName 
			instVarNames: ivNames 
			classVarNames: cvNames
			poolDictionaryNames: #()
			classInstVarNames: civNames
			comment: '').
		(cvNames, ivNames) do: [:ivName | 
			definitions addAll: {
					MCMethodDefinition 
						className: className asString
						selector: ivName asSymbol
						category: 'cat'
						timeStamp: ''
						source: ivName, ' ^', ivName.
					MCMethodDefinition 
						className: className asString
						selector: (ivName, ':') asSymbol
						category: 'cat'
						timeStamp: ''
						source: ivName, ':  anObject ', ivName, ' := anObject'.
				} ].
		civNames do: [:ivName | 
			definitions addAll: {
					MCMethodDefinition 
						className: className asString
						selector: ivName asSymbol
						category: 'cat'
						timeStamp: ''
						source: ivName, ' ^self class ', ivName.
					MCMethodDefinition 
						className: className asString
						selector: (ivName, ':') asSymbol
						category: 'cat'
						timeStamp: ''
						source: ivName, ':  anObject self class ', ivName, ': anObject'.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: ivName asSymbol
						category: 'cat'
						timeStamp: ''
						source: ivName, ' ^', ivName.
					MCMethodDefinition 
						className: className asString
						classIsMeta: true
						selector: (ivName, ':') asSymbol
						category: 'cat'
						timeStamp: ''
						source: ivName, ':  anObject ', ivName, ' := anObject'.
				} ].
		ivNames do: [:ivName|
			definitions add: 
					(MCMethodDefinition 
						className: className asString
						selector: #sampleValue
						category: 'cat'
						timeStamp: ''
						source: 'sampleValue ^#''', ivName, '''') ].
		].
	1 to: methodDefArray size by: 3 do: [:index |
		className := (methodDefArray at: index) asSymbol.
		methodSelector := (methodDefArray at: index + 1) asSymbol.
		methodSource := (methodDefArray at: index + 2).
		definitions add: 
					(MCMethodDefinition 
						className: className asString
						selector: methodSelector
						category: ('*', packageName) asString
						timeStamp: ''
						source: methodSource).
		].
	monticelloRepository basicStoreVersion: (MCVersion new
		setPackage:  (MCPackage new name: basePackageName)
		info: (versionInfo := MCVersionInfo
				name: packageName
				id: UUID new
				message: 'This is a mock version'
				date: Date today
				time: Time now
				author: authorName
				ancestors: ancestors)
		snapshot: (MCSnapshot fromDefinitions: definitions asArray)
		dependencies: #()).
	^versionInfo

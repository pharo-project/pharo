Extension { #name : #MCFileTreeRepository }

{ #category : #'*Ring-Monticello' }
MCFileTreeRepository >> asRingEnvironment [

	| anRGEnvironment |
	anRGEnvironment := RGEnvironment new.
	anRGEnvironment announcer suspendAllWhile: [ 
		self allFileNames do: [ :pn | 
			| snap defs package version |
			package := anRGEnvironment ensurePackageNamed: (pn withoutSuffix: '.package') asSymbol.
			version := self loadVersionFromFileNamed: pn.
			snap := version snapshot.

			defs := snap definitions.
			defs do: [ :each |			
				each ensureRingDefinitionIn: anRGEnvironment package: package ]
			displayingProgress: 'snapshot'.] 
		 displayingProgress: 'versions'.
		].		
			
	anRGEnvironment clean.
	anRGEnvironment behaviorsDo: [ :beh |
		beh superclass == beh ifTrue: [ beh superclass: nil. ] ].			
	^ anRGEnvironment 
]

{ #category : #'*Ring-Monticello' }
MCFileTreeRepository >> asRingEnvironmentWith: packageNames [

	| anRGEnvironment |
	anRGEnvironment := RGEnvironment new.
	anRGEnvironment announcer suspendAllWhile: [ 
		(packageNames) do: [ :pn | 
			| snap defs package version |
			package := anRGEnvironment ensurePackageNamed: pn asSymbol.
			version := self loadVersionFromFileNamed: pn, '.package'.
			snap := version snapshot.

			defs := snap definitions.
			defs do: [ :each |			
				each ensureRingDefinitionIn: anRGEnvironment package: package ]
			displayingProgress: 'snap'.] 
		 displayingProgress: 'version'.
		].		
			
	anRGEnvironment clean.
	anRGEnvironment behaviorsDo: [ :beh |
		beh superclass == beh ifTrue: [ beh superclass: nil. ] ].			
	^ anRGEnvironment 
]

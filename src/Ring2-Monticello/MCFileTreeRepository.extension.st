Extension { #name : #MCFileTreeRepository }

{ #category : #'*Ring2-Monticello' }
MCFileTreeRepository >> asRing2Environment [

	| aRG2Environment |
	aRG2Environment := RG2Environment new.
	aRG2Environment announcer suspendAllWhile: [ 
		self allFileNames do: [ :pn | 
			| snap defs package version |
			package := aRG2Environment ensurePackageNamed: (pn withoutSuffix: '.package') asSymbol.
			version := self loadVersionFromFileNamed: pn.
			snap := version snapshot.

			defs := snap definitions.
			defs do: [ :each |			
				each ensureRing2DefinitionIn: aRG2Environment package: package ]
			displayingProgress: 'snapshot'.] 
		 displayingProgress: 'versions'.
		].		
			
	aRG2Environment clean.
	aRG2Environment behaviorsDo: [ :beh |
		beh superclass == beh ifTrue: [ beh superclass: nil. ] ].			
	^ aRG2Environment 
]

{ #category : #'*Ring2-Monticello' }
MCFileTreeRepository >> asRing2EnvironmentWith: packageNames [

	| aRG2Environment |
	aRG2Environment := RG2Environment new.
	aRG2Environment announcer suspendAllWhile: [ 
		(packageNames) do: [ :pn | 
			| snap defs package version |
			package := aRG2Environment ensurePackageNamed: pn asSymbol.
			version := self loadVersionFromFileNamed: pn, '.package'.
			snap := version snapshot.

			defs := snap definitions.
			defs do: [ :each |			
				each ensureRing2DefinitionIn: aRG2Environment package: package ]
			displayingProgress: 'snap'.] 
		 displayingProgress: 'version'.
		].		
			
	aRG2Environment clean.
	aRG2Environment behaviorsDo: [ :beh |
		beh superclass == beh ifTrue: [ beh superclass: nil. ] ].			
	^ aRG2Environment 
]

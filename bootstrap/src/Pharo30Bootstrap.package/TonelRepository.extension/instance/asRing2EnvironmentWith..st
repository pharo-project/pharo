*Pharo30Bootstrap
asRing2EnvironmentWith: packageNames

	| aRG2Environment |
	aRG2Environment := RG2Environment new.
	aRG2Environment announcer suspendAllWhile: [ 
		(packageNames) do: [ :pn | 
			| snap defs package version |
			package := aRG2Environment ensurePackageNamed: pn asSymbol.
			version := self versionWithInfo: (self versionInfoFromVersionNamed: pn).
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

Extension { #name : #TonelRepository }

{ #category : #'*Ring-Monticello' }
TonelRepository >> asRingEnvironmentWith: packageNames [

	^ self asRingEnvironmentWith: packageNames cleaning: true
]

{ #category : #'*Ring-Monticello' }
TonelRepository >> asRingEnvironmentWith: packageNames cleaning: aBoolen [

	| aRG2Environment |
	aRG2Environment := RGEnvironment new.
	aRG2Environment announcer suspendAllWhile: [ 
		(packageNames) do: [ :pn | 
			| snap defs package version |
			package := aRG2Environment ensurePackageNamed: pn asSymbol.
			version := self versionWithInfo: (self versionInfoFromVersionNamed: pn).
			snap := version snapshot.

			defs := snap definitions.
			defs do: [ :each |			
				each ensureRingDefinitionIn: aRG2Environment package: package ]
			displayingProgress: 'snap'.] 
		 displayingProgress: 'version'.
		].		
			
	aBoolen ifTrue: [ 
		aRG2Environment clean ].
	
	aRG2Environment behaviorsDo: [ :beh |
		beh superclass == beh ifTrue: [ beh superclass: nil. ] ].			
	^ aRG2Environment
]

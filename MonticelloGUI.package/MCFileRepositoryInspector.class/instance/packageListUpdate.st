morphic ui
packageListUpdate
	| result packageNames |	
	
	"Within fork, make no changes to object state until end with assignment wrapped by #defer:"
	
	packageListUpdateProcess ifNotNil: [ packageListUpdateProcess terminate ].	
	packageListUpdateProcess := 
	[	versions ifNotNil: 
		[	result := Set new: versions size.
			versions do: [ :each | result add: each first ].
			
			"sort loaded packages first, then alphabetically"
			packageNames := Set new: loaded size.
			loaded do: [ :each |
				packageNames add: (each copyUpToLast: $-). ].
			
			result := result asArray sort: [ :a :b |
				| loadedA loadedB |
				loadedA := packageNames includes: a.
				loadedB := packageNames includes: b.
				loadedA = loadedB 
					ifTrue: [ a < b ]
					ifFalse: [ loadedA ] ].
	
			packagePattern ifNotEmpty: [
				result := result select: [ :packageName |  packageName asLowercase includesSubstring: packagePattern ]].
			 
			UIManager default defer:
			[	packageList := result.
				self changed: #packageList.
				self changed: #versionList.
			]
		]
	] fork.
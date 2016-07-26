protocol
commitKomit: aKomit
	| versions |
	
	versions := self createVersionFor: aKomit.
	
	versions do: [ :version | 
		Cursor wait showWhile: [ 
			[ self
				storeVersion: version for: aKomit;
				storeDependencies: version  for: aKomit
			] ensure: [ (MCVersionInspector new version: version) show ] 
		]
	 ]
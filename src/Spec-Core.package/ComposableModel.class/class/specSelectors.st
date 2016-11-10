protocol
specSelectors
	^ self class withAllSuperclasses
		flatCollect: [ :class | 
			(((SpecPragmaCollector behavior: class)
				filter: [ :pragma | pragma selector = #spec ])
				reset;
				collected) collect: [ :pragmas | pragmas method selector ] ]
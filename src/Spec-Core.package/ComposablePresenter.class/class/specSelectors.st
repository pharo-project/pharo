protocol
specSelectors
	
	^  self class withAllSuperclasses flatCollect: [ :class |
		(((SpecPragmaCollector behavior: class)
			filter: [ :pragma | pragma keyword = #spec])
			reset;
			collected)
			collect: [ :pragmas | pragmas method selector ]]
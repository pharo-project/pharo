accessing
allTags

	^ (self allPragmasWithTag collect: [ :pragma | 
		pragma argumentAt: 1 ]) asSet sorted: [ :a :b | a < b ]
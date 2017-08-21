private
hasNoDependenciesFor: aClass in: unprocessedClasses cache: cache 
	^ (self hasNoSuperclassesOf: aClass in: unprocessedClasses cache: cache) and: [
		aClass isMeta not or: [
			self hasNoDependenciesForMetaclass: aClass in: unprocessedClasses cache: cache]] 
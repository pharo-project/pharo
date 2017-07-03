fileIn/Out
hasNoSuperclassesOf: aClass in: unprocessedClasses cache: cache 
	^ (unprocessedClasses includesAnyOf: (self allSuperclassesFor: aClass cache: cache)) not
	
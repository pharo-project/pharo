class initialization
ensureInSpecialObjectsArray
	"Alien must be in the specialObjectsArray to enable the Newsqueak FFI.  Because of
	 the bootstrap the specialObjectsArray may already have been partially initialized with
	 the callback selector and to be large enough (as part of loading immutability), but
	 it will be missing Alien until Alien is loaded.  So check if the specialObjectsArray is
	 of the expected size before recreating and slam Alien in if the specialObjectsArray
	 is already large enough."

	| index |
	self ~~ Alien ifTrue: [^self].

	index := 53.
	((Smalltalk includesKey: #VMObjectIndices)
	 and: [((Smalltalk at: #VMObjectIndices) classPool at: #ClassAlien ifAbsent: []) ~= (index - 1)]) ifTrue:
		[self error: 'index probably wrong'].

	Smalltalk specialObjectsArray size < index ifTrue:
		[Smalltalk recreateSpecialObjectsArray].
	
	(Smalltalk specialObjectsArray size < index
	 or: [(Smalltalk specialObjectsArray at: index) ~~ nil
		and: [(Smalltalk specialObjectsArray at: index) ~~ self]]) ifTrue:
		[self error: 'SystemDictionary>>recreateSpecialObjectsArray appears incompatible'].

	(Smalltalk specialObjectsArray at: index) ifNil:
		[Smalltalk specialObjectsArray at: index put: self]
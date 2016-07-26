class initialization
ensureInSpecialObjectsArray
	"UnsafeAlien must be in the specialObjectsArray to enable the Newsqueak FFI.
	 Because of the bootstrap the specialObjectsArray may already have been
	 partially initialized with the callback selector and to be large enough (as part of
	 loading immutability), but it will be missing UnsafeAlien until UnsafeAlien is loaded.
	 So check if the specialObjectsArray is of the expected size before recreating
	 and slam UnsafeAlien in if the specialObjectsArray is already large enough."

	| index |
	self ~~ UnsafeAlien ifTrue: [^self].

	index := 55.
	((Smalltalk includesKey: #ObjectMemory)
	 and: [((Smalltalk at: #ObjectMemory) classPool at: #ClassUnsafeAlien ifAbsent: []) ~~ (index - 1)]) ifTrue:
		[self error: 'index probably wrong'].

	Smalltalk specialObjectsArray size < index ifTrue:
		[Smalltalk recreateSpecialObjectsArray].
	
	(Smalltalk specialObjectsArray size < index
	 or: [(Smalltalk specialObjectsArray at: index) ~~ nil
		and: [(Smalltalk specialObjectsArray at: index) ~~ self]]) ifTrue:
		[self error: 'SystemDictionary>>recreateSpecialObjectsArray appears incompatible'].

	(Smalltalk specialObjectsArray at: index) == nil ifTrue:
		[Smalltalk specialObjectsArray at: index put: self]
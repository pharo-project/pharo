copying
postCopy
	"should we prohibit any attempts to copy receiver?"
	self protected: [ | oldDict |
		sema := Semaphore forMutualExclusion.
		oldDict := valueDictionary.
		list := WeakFinalizationList new.
		valueDictionary := WeakIdentityKeyDictionary new.
		self installFinalizer.
	
		oldDict keysAndValuesDo: [:key :value |
			valueDictionary at: key put: (value copyWithList: list)
		].
	]
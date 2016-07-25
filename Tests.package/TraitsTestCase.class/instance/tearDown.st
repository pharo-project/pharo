running
tearDown
	TraitsResource reset.
	self createdClassesAndTraits
		do: [ :aClassOrTrait || behaviorName |
			behaviorName := aClassOrTrait name.
			Smalltalk globals at: behaviorName ifPresent: [ :classOrTrait | classOrTrait removeFromSystem ].
			ChangeSet current removeClassChanges: behaviorName ].
	createdClassesAndTraits := nil.
	SystemAnnouncer announcer: oldAnnouncer.

running
tearDown
	self createdClassesAndTraits
		do: [ :aClassOrTrait | 
			| behaviorName |
			behaviorName := aClassOrTrait name.
			Smalltalk globals at: behaviorName ifPresent: [ :classOrTrait | classOrTrait removeFromSystem ].
			ChangeSet current removeClassChanges: behaviorName ].
	createdClassesAndTraits := self
		t1:
			(self
				t2:
					(self
						t3:
							(self
								t4:
									(self t5: (self t6: (self c1: (self c2: (self c3: (self c4: (self c5: (self c6: (self c7: (self c8: nil))))))))))))).
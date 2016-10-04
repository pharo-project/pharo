accessing
identityOrderedAssociations
	"Returns ordered associations to use for identity dictionaries.
	The keys are all #= equal but #== different, so only an
	identity dictionary will be able to distinguish them."
	identityOrderedAssociations
		ifNil: [| key |
			key := 'testKey'.
			identityOrderedAssociations :=
				Array
					with: (key := key copy) -> 'testValue'
					with: (key := key copy) -> 'testValue3'
					with: (key := key copy) -> 'testValue2'
					with: (key := key copy) -> 'testValue4'].
	"return copies of the associations so they can be safely modified
	in one test without affecting another, but do not copy the keys
	and values"
	^ identityOrderedAssociations collect: [:each | each copy].
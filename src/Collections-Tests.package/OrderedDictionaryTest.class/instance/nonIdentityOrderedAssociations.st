accessing
nonIdentityOrderedAssociations
	"Returns ordered associations to use for non-identity dictionaries.
	The keys are all #= and #== different and are returned out of their
	natural sort order."
	nonIdentityOrderedAssociations
		ifNil: [
			nonIdentityOrderedAssociations :=
				Array
					with: 'testKey' -> 'testValue'
					with: 'testKey3' -> 'testValue3'
					with: 'testKey2' -> 'testValue2'
					with: 'testKey4' -> 'testValue4'].
	"return copies of the associations so they can be safely modified
	in one test without affecting another, but do not copy the keys
	and values"
	^ nonIdentityOrderedAssociations collect: [:each | each copy].
accessing
identityDictionaryAssociations
	"associations are copied so can be modified in one test without affecting others,
	but the keys and values are identical each time for identity tests."
	dictionaryAssociations
		ifNil: [| key |
			key := 'testKey'.
			dictionaryAssociations :=
				Array
					with: (key := key copy) -> 'testValue'
					with: (key := key copy) -> 'testValue3'
					with: (key := key copy) -> 'testValue2'
					with: (key := key copy) -> 'testValue4'].
	^ dictionaryAssociations collect: [:each | each copy].
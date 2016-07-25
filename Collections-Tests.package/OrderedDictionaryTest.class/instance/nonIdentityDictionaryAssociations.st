accessing
nonIdentityDictionaryAssociations
	"Associations are grouped out of their natural sort order and
	are copied so can be modified in one test without affecting others,
	but the keys and values are identical each time for identity tests."
	dictionaryAssociations ifNil: [
		dictionaryAssociations :=
		Array
			with: 'testKey'->'testValue'
			with: 'testKey3'->'testValue3'
			with: 'testKey2'->'testValue2'
			with: 'testKey4'->'testValue4'].
	^ dictionaryAssociations collect: [:each | each copy].
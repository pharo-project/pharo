initialization
initialize

	super initialize.
	primitivesBucket := IdentityDictionary new.
	baselevelBucket := IdentityDictionary new.
	globalsBucket := IdentityDictionary new.
	metalevelInstanceSideBucket := IdentityDictionary new.
	metalevelClassSideBucket := IdentityDictionary new.
	substitutionsBucket := IdentityDictionary new.
	postBaselevelBucket := IdentityDictionary new.

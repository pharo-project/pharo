comparing
= another
	^ self species = another species and: [ self referencesSameRemoteLocationAs: another ]
	
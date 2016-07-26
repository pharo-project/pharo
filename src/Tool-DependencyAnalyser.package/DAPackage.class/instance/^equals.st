comparing
= aPackage
	(aPackage isKindOf: self class)
		ifFalse: [ ^ false ].
	^ self packageName = aPackage packageName
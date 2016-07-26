comparing
= aCookie
	(aCookie isKindOf: self class) ifFalse: [ ^ false ].
	^ (aCookie name = self name) & (aCookie path = self path) & (aCookie domain = self domain)
	
as yet unclassified
in: aSeq allSharedPoolsAreBeforeThan: aClass
	| myIndex allSharedPools |
	myIndex := aSeq indexOf: aClass.
	allSharedPools := ({aClass} , (aClass allSuperclassesIn: aSeq)) flatCollect: [:e | e sharedPoolsIn: aSeq].
	
	^ allSharedPools allSatisfy: [ :sp | (aSeq indexOf: sp) < myIndex ]
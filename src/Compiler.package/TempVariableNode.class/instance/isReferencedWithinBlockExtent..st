code generation (closures)
isReferencedWithinBlockExtent: anInterval 
	readingScopes ~~ nil ifTrue:
		[readingScopes do:
			[:set "<Set of <Integer>>"|
			set do:
				[:location|
				 (anInterval rangeIncludes: location) ifTrue:
					[^true]]]].
	writingScopes ~~ nil ifTrue:
		[writingScopes do:
			[:set "<Set of <Integer>>"|
			set do:
				[:location|
				 (anInterval rangeIncludes: location) ifTrue:
					[^true]]]].
	^false
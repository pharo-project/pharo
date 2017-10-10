querying
sharedPoolsIn: aCollection
	^ ((sharedPools substrings collect: #asSymbol) collect: [ :aName | aCollection detect: [ :aClass | aClass className = aName ] ifNone: nil ]) reject: #isNil
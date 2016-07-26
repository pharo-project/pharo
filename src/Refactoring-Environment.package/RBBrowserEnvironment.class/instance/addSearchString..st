accessing
addSearchString: aString 
	searchStrings isNil ifTrue: [
		searchStrings := SortedCollection sortBlock: [ :a :b | 
			(a indexOf: $: ifAbsent: [ a size ]) > (b indexOf: $: ifAbsent: [ b size ]) ] ].
	(searchStrings includes: aString)
		ifFalse: [ searchStrings add: aString ]
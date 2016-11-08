comparing
matches: aString

	| idx |
	
	pattern size > aString size ifTrue: [ ^ false ].

	idx := 0.
	pattern withIndexDo: [ :each :i |
		idx := aString 
			findString: each asString 
			startingAt: idx + 1 
			caseSensitive: false. 
		
		idx == 0 ifTrue: [ ^ false ].
		indexes at: i put: idx.
	].

	^ true
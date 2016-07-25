merging
mergeSpec: anotherSpec

	| newSpec map anotherRequires anotherIncludes anotherAnswers |
	newSpec := super mergeSpec: anotherSpec.
	map := anotherSpec mergeMap.
	anotherSpec name ~~ nil 
		ifTrue: [ newSpec name: anotherSpec name ].
	(anotherRequires := map at: #requires) ~~ nil
		ifTrue: [ newSpec setRequires: self requires, anotherRequires ].
	(anotherIncludes := map at: #includes) ~~ nil
		ifTrue: [ newSpec setIncludes: self includes, anotherIncludes ].
	(anotherAnswers := map at: #answers) ~~ nil
		ifTrue: [ newSpec setAnswers: self answers, anotherAnswers ].
	^newSpec
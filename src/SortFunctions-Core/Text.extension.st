Extension { #name : 'Text' }

{ #category : '*SortFunctions-Core' }
Text >> threeWayCompareTo: anotherText [
	
	^ self asString threeWayCompareTo: anotherText asString
]

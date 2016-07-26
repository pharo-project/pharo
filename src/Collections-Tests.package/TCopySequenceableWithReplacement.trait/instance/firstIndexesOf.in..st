tests - copying with replacement
firstIndexesOf: aSubCollection in: collection
" return an OrderedCollection with the first indexes of the occurrences of subCollection in  collection "
	| tmp result currentIndex |
	tmp:= collection.
	result:= OrderedCollection new.
	currentIndex := 1.
	
	[tmp isEmpty ]whileFalse:
		[
		(tmp beginsWith: aSubCollection)
			ifTrue: [ 	
				result add: currentIndex.
				1 to: aSubCollection size do: 
					[:i | 
					tmp := tmp copyWithoutFirst.
					currentIndex := currentIndex + 1]
				]
			ifFalse: [
				tmp := tmp copyWithoutFirst.
				currentIndex := currentIndex +1.
				]
		 ].
	
	^ result.
	
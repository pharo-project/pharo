testing
hasRepositoryUrl
	^ self arguments size > 0 and: [ 
		| possibleUrl |
		possibleUrl := self arguments first.
		"check if the first argument is some sort of a URL"
		(possibleUrl includesSubstring: '://') or: [ possibleUrl first = $. ]]
private
findPathIn: anArray 
	| found |
	found := self findExactPathMatchIn: anArray.
	found
		ifNil: ["try again with no case sensitivity"
			found := self findSimilarPathMatchIn: anArray].
	^ found
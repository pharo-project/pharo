tests - replacing
testReplaceAllWith
	| result  collection oldElement newElement oldOccurrences |
	collection := self nonEmpty .
	result := collection  copy.
	oldElement := self elementInForReplacement .
	newElement := self newElement .
	oldOccurrences := (result occurrencesOf: oldElement) + (result occurrencesOf: newElement).
	result replaceAll: oldElement  with: newElement  .
	
	self assert: oldOccurrences = (result occurrencesOf: newElement)
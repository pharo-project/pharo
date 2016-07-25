converting
correctAgainst: wordList
	"Correct the receiver: assume it is a misspelled word and return the (maximum of five) nearest words in the wordList.  Depends on the scoring scheme of alike:"
	| results |
	results := self correctAgainst: wordList continuedFrom: nil.
	results := self correctAgainst: nil continuedFrom: results.
	^ results
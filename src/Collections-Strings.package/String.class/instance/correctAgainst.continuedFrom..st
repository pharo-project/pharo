converting
correctAgainst: wordList continuedFrom: oldCollection
	"Like correctAgainst:.  Use when you want to correct against several lists, give nil as the first oldCollection, and nil as the last wordList."

	^ wordList 
		ifNil: [ self correctAgainstEnumerator: nil
					continuedFrom: oldCollection ]
		ifNotNil: [ self correctAgainstEnumerator: [ :action | wordList do: action without: nil]
					continuedFrom: oldCollection ]
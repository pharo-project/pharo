converting
correctAgainstDictionary: wordDict continuedFrom: oldCollection
	"Like correctAgainst:continuedFrom:.  Use when you want to correct against a dictionary."

	^ wordDict 
		ifNil: [ self correctAgainstEnumerator: nil
					continuedFrom: oldCollection ]
		ifNotNil: [ self correctAgainstEnumerator: [ :action | wordDict keysDo: action ]
					continuedFrom: oldCollection ]
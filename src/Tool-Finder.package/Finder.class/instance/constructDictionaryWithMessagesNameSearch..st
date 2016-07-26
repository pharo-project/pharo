private-selector
constructDictionaryWithMessagesNameSearch: aString
	"Construct dictionary when searching for selector"
	
	| result listOfMethods |
	result := Dictionary new.
	listOfMethods := self messagesNameSearch: aString.
	listOfMethods do: [:method || key value |
			key := method selector.
			value := method methodClass.
			(result includesKey: key)
				ifTrue: [ (result at: key) add: value]
				ifFalse: [ result at: key put: (OrderedCollection new add: value; yourself)]].
	self resultDictionary: result.
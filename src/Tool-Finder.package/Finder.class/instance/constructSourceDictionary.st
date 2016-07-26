private
constructSourceDictionary
"construct dictionary when searching source"
	| result listOfMethods |
	result := Dictionary new.
	listOfMethods := self sourceSearch: self searchingString.
	listOfMethods do: [:method || key value |
		key := method selector.
		value := method methodClass.
		(result includesKey: key)
			ifTrue: [ (result at: key) add: value]
			ifFalse: [ result at: key put: (OrderedCollection new add: value; yourself)]].
	self resultDictionary: result.
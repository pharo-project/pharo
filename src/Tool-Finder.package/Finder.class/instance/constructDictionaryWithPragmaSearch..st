private-pragma
constructDictionaryWithPragmaSearch: aString 
	"construct dictionary when searching for pragmas"
	
	| dictionary |
	dictionary := self pragmaSearch: aString.
	dictionary keysDo:[ :k || result |
		result := Dictionary new.
		(dictionary at: k) do: [:method || key value |
				key := method selector.
				value := method methodClass.
				(result 
					at: key
					ifAbsentPut: OrderedCollection new ) add: value].
		dictionary at: k put: result ].
	self resultDictionary: dictionary.
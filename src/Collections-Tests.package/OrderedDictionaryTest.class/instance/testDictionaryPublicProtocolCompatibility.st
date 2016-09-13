tests
testDictionaryPublicProtocolCompatibility
	| dictionary |

	dictionary := self emptyDictionary.
	{dictionary -> Dictionary.
	dictionary class -> Dictionary class}
		do: [:assoc | 
			assoc value protocols
				reject: [:protocol | 
					#('private' 'print' 'copy' 'compar' '*')
						anySatisfy: [:each | protocol asString beginsWith: each]]
				thenDo: [:protocol | 
					(assoc value selectorsInProtocol: protocol)
						do: [:each | self assert: (assoc key respondsTo: each)]]].
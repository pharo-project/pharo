private
withMaximumNumberOfDictionaryEntriesDo: block
	^ self maximumNumberOfDictionaryEntries = ZnMaximumNumberOfDictionaryEntries value
			ifTrue: block
			ifFalse: [ 
				ZnMaximumNumberOfDictionaryEntries 
					value: self maximumNumberOfDictionaryEntries 
					during: block ]
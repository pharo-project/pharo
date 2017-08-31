options
maximumNumberOfDictionaryEntries
	"Return the maximum number of entries allowed in ZnMutliValueDictionaries before signalling ZnTooManyDictionaryEntries. This protects us from overflow attacks."
		
	^ self
		optionAt: #maximumNumberOfDictionaryEntries
		ifAbsent: [ ZnMaximumNumberOfDictionaryEntries value ]
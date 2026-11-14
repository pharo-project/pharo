Extension { #name : 'ZnMaximumNumberOfDictionaryEntries' }

{ #category : '*Deprecated15' }
ZnMaximumNumberOfDictionaryEntries class >> value [
	"Deprecated in Pharo 15: this is now the #maximumNumberOfDictionaryEntries ZnOption.
	Binding this variable no longer has any effect on Zinc."

	self
		deprecated: 'Use the #maximumNumberOfDictionaryEntries ZnOption instead'
		transformWith: '`@rcv value' -> '(ZnCurrentOptions at: #maximumNumberOfDictionaryEntries)'.
	^ ZnCurrentOptions at: #maximumNumberOfDictionaryEntries
]

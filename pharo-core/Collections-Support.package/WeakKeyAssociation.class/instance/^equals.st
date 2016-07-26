comparing
= aLookupKey

	"XXX why doesn't this consider the value? mvl"

	self species = aLookupKey species
		ifTrue: [^self key = aLookupKey key]
		ifFalse: [^false]
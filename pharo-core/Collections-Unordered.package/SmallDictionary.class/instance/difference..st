enumerating - keys and values
difference: aCollection
	"Answer the set theoretic difference of two collections. This is a specialized version for Dictionaries keeping the keys of the objects. At a slightly higher price of an additional Set to track duplicates."

	| other result duplicates |
	
	other := aCollection asSet.
	duplicates := Set new.
	result := self class new: self size.
	
	self keysAndValuesDo: [ :key :value|
		((other includes: value) not and: [ (duplicates includes: value) not ])
			ifTrue: [ 
				duplicates add: value.
				result at: key put: value]].
	
	^ result
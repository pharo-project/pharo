tests - index accessing for multipliness
testIdentityIndexOfDuplicate
	"self debug: #testIdentityIndexOf"

	| collection element |
	"testing fixture here as this method may not be used by some collections testClass"
	self collectionWithNonIdentitySameAtEndAndBegining.
	collection := self collectionWithNonIdentitySameAtEndAndBegining.
	self assert: collection first = collection last.
	self deny: collection first == collection last.
	1 to: collection size do: [ :i | 
		i > 1 & (i < collection size)
			ifTrue: [ self deny: (collection at: i) = collection first ] ].
	element := collection last.	" floatCollectionWithSameAtEndAndBegining first and last elements are equals but are not the same object"
	self assert: (collection identityIndexOf: element) = collection size
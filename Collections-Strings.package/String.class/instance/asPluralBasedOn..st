converting
asPluralBasedOn: aNumberOrCollection
	"Append an 's' to this string based on whether aNumberOrCollection is 1 or of size 1."

	^ (aNumberOrCollection = 1 or:
		[aNumberOrCollection isCollection and: [aNumberOrCollection size = 1]])
			ifTrue: [self]
			ifFalse: [self, 's']

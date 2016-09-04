accessing
upToEnd
	"Answer a subcollection from the current access position through the last element of the receiver."

	^self collectionSpecies streamContents: [ :newStream |
		| next |
		[ (next := self next) == nil ] whileFalse: [
			newStream nextPut: next ] ]
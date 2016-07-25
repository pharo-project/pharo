private
noCheckAdd: anObject
	"This method should be deprecated"
	array at: (self findElementOrNil: anObject) put: anObject asSetElement.
	tally := tally + 1
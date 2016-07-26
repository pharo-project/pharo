private
noCheckAdd: anObject
	"Must be defined separately for Dictionary because (self findElementOrNil:) expects a key, not an association."

	array at: (self findElementOrNil: anObject key) put: anObject.
	tally := tally + 1
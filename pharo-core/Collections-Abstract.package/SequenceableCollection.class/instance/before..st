accessing
before: target
	"Answer the receiver's element immediately before target. Raise an
	error if target is not an element of the receiver, or if there are no 
	elements before it (i.e. it is the first element)."

	^ self before: target ifAbsent: [self errorNotFound: target]
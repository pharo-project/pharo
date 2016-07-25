accessing
after: target
	"Answer the element after target.  Raise an error if target is not
	in the receiver, or if there are no elements after it."

	^ self after: target ifAbsent: [self errorNotFound: target]
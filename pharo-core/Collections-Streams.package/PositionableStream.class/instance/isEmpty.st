testing
isEmpty
	"Answer whether the receiver's contents has no elements."

	"Returns true if both the set of past and future sequence values of
the receiver are empty. Otherwise returns false"

	^ self atEnd and: [position = 0]
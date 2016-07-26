instance creation
ofSize: n
	"Create a new collection of size n with nil as its elements.
	This method exists because OrderedCollection new: n creates an
	empty collection,  not one of size n."
	^ self new: n
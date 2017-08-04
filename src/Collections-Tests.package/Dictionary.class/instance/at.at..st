accessing nested
at: firstKey at: secondKey
	"Return the object stored in the second dictionary returned by firstKey."

	"
	[
	| d |
	d := Dictionary new.
	d at: #top at: #below1 put: 1.
	d at: #top at: #below1 put: 2.
	d at: #top at: #below1.
	] value
	>>>
	2"

	^ self at: firstKey at: secondKey ifAbsent: [ self errorKeyNotFound ]
nested dictionaries
at: firstKey at: secondKey
	"Return the object stored in the second dictionary at secondKey. The second dictionary is accessed via the key firstKey."

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
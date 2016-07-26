accessing
upToEnd
	"answer the remaining elements in the string"
	| elements |
	elements := OrderedCollection new.
	[ self atEnd ] whileFalse: [ 
		elements add: self next ].
	^elements
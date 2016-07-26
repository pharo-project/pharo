debugger support
blockExtentsToTempsMap
	"Answer a Dictionary of blockExtent to temp locations for the current method.
	 This is used by the debugger to locate temp vars in contexts.  A temp map
	 entry is a pair of the temp's name and its index, where an index is either an
	 integer for a normal temp or a pair of the index of the indirect temp vector
	 containing  the temp and the index of the temp in its indirect temp vector."

	^encoder blockExtentsToTempsMap ifNil:
		[| methNode |
		methNode := Parser new
						encoderClass: encoder class;
						parse: self sourceCode
						class: self methodClass.
		"As a side effect generate: creates data needed for the map."
		methNode generate.
		methNode encoder blockExtentsToTempsMap]
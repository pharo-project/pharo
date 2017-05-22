accessing
strings
	| externalArray |
	self count = 0
		ifTrue: [ ^ #() ].
	externalArray := LGitExternalStringArray 
		onAddress: self prim_strings getHandle 
		size: self count.
	^externalArray asArray
accessing
strings: aCollection
	| array |
	array := LGitExternalStringArray fromCollection: aCollection.
	self prim_strings: array
copying
postCopy
	origins := self originReferences collect: [:each | each copy ].
	destination := destination copy.
	transmissionStrategy := transmissionStrategy copy.
	transmissionStrategy transmission: self
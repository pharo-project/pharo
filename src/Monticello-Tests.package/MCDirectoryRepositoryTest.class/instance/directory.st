accessing
directory
	directory ifNil:
		[directory := 'mctest' asFileReference.
		directory ensureCreateDirectory].
	^ directory
copying
postCopy
	super postCopy.
	monitor critical:
		[items := items copy.
		monitor := Monitor new]